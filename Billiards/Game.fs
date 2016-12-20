namespace Billiards
open Billiards.HelperFunctions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open System

type BallId =
    | BallCue | Ball1 | Ball2 | Ball3 | Ball4 | Ball5 | Ball6 | Ball7
    | Ball8 | Ball9 | Ball10 | Ball11 | Ball12 | Ball13 | Ball14 | Ball15

type Sprite =
    | Ball of BallId
    | CornerPocket | SidePocketV | SidePocketH
    | Table

type Pocket = { position: Vector2; hflip: bool; vflip: bool; sprite: Sprite }

type Ball = { position: Vector2; velocity: Vector2; id: BallId }

type GameState = | Aiming of float32 | Settling

/// A collision body
type Body =
    | Ball of Ball
    | HorizontalWall
    | VerticalWall
    | Wall

type Game =
    { state: GameState
      walls: (Vector2 * Vector2) list
      balls: Ball list
      pockets: Pocket list }

type CollisionManifold = { body1: Body; body2: Body; penetration: Vector2; normal: Vector2 }

module Constants =
    /// in pixels per meter
    let scale = 400.f
    /// In meters (from 106x56in)
    let tableLength, tableWidth = 2.6924f, 1.4224f
    /// In meters (from 100x50in)
    let surfaceLength, surfaceWidth = 2.54f, 1.27f
    let cushionWidth = (tableLength - surfaceLength) / 2.f
    /// In meters (from 2.25in diameter)
    let ballRadius = 0.05715f / 2.f
    /// In grams (from 6oz)
    let ballMass = 170.097f
    /// Represents a continuous friction coefficient. I guessed at a value for this.
    let ballSurfaceFriction = 1.f
    let minVelocity = 0.01f
    /// In meters
    let pocketWidth = 35.f / scale
    
    let viewMatrix =
        Matrix.CreateScale (1.f, -1.f, 1.f)
        * Matrix.CreateTranslation (cushionWidth, cushionWidth + surfaceWidth, 0.f)
        * Matrix.CreateScale scale
    let inverseViewMatrix = Matrix.Invert viewMatrix
    
    let world2ScreenCoords v = Vector2.Transform (v, viewMatrix)
    let screen2WorldCoords v = Vector2.Transform (v, inverseViewMatrix)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ball =
    let stepForwardX (timeDelta: float32) (position: Vector2) velocity =
        position.X + (velocity * timeDelta)
    
    let stepForwardY (timeDelta: float32) (position: Vector2) velocity =
        position.Y + (velocity * timeDelta)
    
    let stepForward timeDelta position velocity =
        stepForwardX timeDelta position velocity @@ stepForwardY timeDelta position velocity
    
    let move (timeDelta: float32) (ball: Ball) = { ball with position = ball.position + (ball.velocity * timeDelta) }
    
    let touchingPocket ball pocket =
        // for now, only handle the top center pocket
        pocket.sprite = SidePocketV && pocket.vflip
        && ball.position.Y + Constants.ballRadius > pocket.position.Y - 0.01f
        && ball.position.X - Constants.ballRadius > pocket.position.X - (Constants.pocketWidth / 2.f)
        && ball.position.X + Constants.ballRadius < pocket.position.X + (Constants.pocketWidth / 2.f)
    
    let predictCollisions (timeDelta: float32) pockets walls (balls: (Ball * Vector2) list) =
        [for (ball1, p1_) in balls do
            for (timeDelta', normal, wallId) in
                [if p1_.Y < Constants.ballRadius && ball1.velocity.Y < 0.f then
                    yield (0.f + Constants.ballRadius - ball1.position.Y) / ball1.velocity.Y, 1 @@ 0, HorizontalWall
                 if p1_.Y > Constants.surfaceWidth - Constants.ballRadius && ball1.velocity.Y > 0.f then
                    yield (Constants.surfaceWidth - Constants.ballRadius - ball1.position.Y) / ball1.velocity.Y, 1 @@ 0, HorizontalWall
                 if p1_.X < Constants.ballRadius && ball1.velocity.X < 0.f then
                    yield (0.f + Constants.ballRadius - ball1.position.X) / ball1.velocity.X, 0 @@ 1, VerticalWall
                 if p1_.X > Constants.surfaceLength - Constants.ballRadius && ball1.velocity.X > 0.f then
                    yield (Constants.surfaceLength - Constants.ballRadius - ball1.position.X) / ball1.velocity.X, 0 @@ 1, VerticalWall]
                do yield { body1 = Ball(ball1); body2 = wallId
                           penetration = p1_ - (ball1.position + (ball1.velocity * timeDelta'))
                           normal = normal }
            
            for (v1, v2) in walls do
                let near = projectToLineSegment (v1, v2, p1_)
                let penetrationDepth = Constants.ballRadius - ((p1_ - near).Length ())
                if penetrationDepth > 0.f then
                    // the normal is really easy here -- just rotate by 90°
                    let normal = normalize (p1_ - near) //normalize -((v1.Y - v2.Y) @@ (v2.X - v1.X))
                    let v1ToV2 = v1 - v2
                    yield { body1 = Ball(ball1); body2 = Wall
                            penetration = -(normal * penetrationDepth)
                            normal = normal }
            
            // ball/ball collisions
            for (ball2, p2_) in balls do
                let p1_MinusP2_ = p1_ - p2_
                let p1_MinusP2_Length = p1_MinusP2_.Length ()
                let p1_MinusP2_Normal = p1_MinusP2_ / p1_MinusP2_Length
                let penetrationDepth = Constants.ballRadius * 2.f - p1_MinusP2_Length
                if penetrationDepth > 0.f && (ball1.id <> ball2.id) then
                    let penetration = -(p1_MinusP2_Normal * penetrationDepth)
                    yield { body1 = Ball(ball1); body2 = Ball(ball2)
                            penetration = penetration
                            normal = p1_MinusP2_Normal } ]
    
    let updateAll (timeDelta: float32) pockets walls balls =
        // 1) calculate future positions based purely on velocity
        // 2) use this to detect collisions and generate information (e.g. between which bodies, penetration depth, normal)
        // 3) use these manifolds to get a net displacement and make the velocities react accordingly
        let balls = balls |> List.map (fun ball -> ball, ball.position + (ball.velocity * timeDelta))
        let collisions = predictCollisions timeDelta pockets walls balls
        // there's lots of room for optimization, but I don't really expect performance to be a problem here
        balls |> List.map (fun (ball1, p1_) ->
            let ball1Collisions = List.filter (fun manifold -> match manifold.body1 with | Ball(ball1') -> ball1.id = ball1'.id | _ -> false) collisions
            //let ball1Collisions = ball1Collisions |> List.filter (fun manifold -> if manifold.body2 = Wall then printfn "%A" manifold.penetration; false else true)
            let netDisplacement =
                -(ball1Collisions |> List.sumBy (fun manifold -> if manifold.body2 = HorizontalWall || manifold.body2 = VerticalWall || manifold.body2 = Wall then manifold.penetration else manifold.penetration / 2.f))
            // just pretend there's only one collision normal for now
            let v1' =
                match ball1Collisions with
                | [] -> ball1.velocity
                | manifold::_ ->
                    match manifold.body2 with
                    | HorizontalWall -> ball1.velocity * (1 @@ -1)
                    | VerticalWall -> ball1.velocity * (-1 @@ 1)
                    | Wall ->
                        // Invert normal velocity (since wall doesn't move)
                        let vn = Vector2.Dot (ball1.velocity, manifold.normal) * manifold.normal
                        let vt = ball1.velocity - vn
                        vt - vn
                    | Ball(ball2) ->
                        // Swap normal velocities
                        let vn1 = Vector2.Dot (ball1.velocity, manifold.normal) * manifold.normal
                        let vn2 = Vector2.Dot (ball2.velocity, manifold.normal) * manifold.normal
                        let vt1 = ball1.velocity - vn1
                        vn2 + vt1
            
            { ball1 with
                velocity = v1' * float32 (Math.Exp (float (-Constants.ballSurfaceFriction * timeDelta)))
                position = p1_ + netDisplacement })

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Game =
    let init () =
        let balls =
            [0 @@ 0, Ball1
             1 @@ 1, Ball2; 1 @@ -1, Ball3
             2 @@ -2, Ball4; 2 @@ 0, Ball8; 2 @@ 2, Ball5
             3 @@ -3, Ball6; 3 @@ -1, Ball7; 3 @@ 1, Ball9; 3 @@ 3, Ball10
             4 @@ -4, Ball11; 4 @@ -2, Ball12; 4 @@ 0, Ball13; 4 @@ 2, Ball14; 4 @@ 4, Ball15]
        let ballsOrigin = 0.7f * Constants.surfaceLength @@ 0.5f * Constants.surfaceWidth
        let ballsPositionScale = sin (1.f / 3.f * pi) @@ cos (1.f / 3.f * pi)
        { state = Aiming(0.f)
          walls = [0 @@ 0, 1 @@ 1]
          balls =
            { position = (0.5f * Constants.surfaceLength) @@ (0.5f * Constants.surfaceWidth); velocity = 0 @@ 0; id = BallCue }
            :: List.map (fun (position, id) ->
                { position = ballsOrigin + (position * ballsPositionScale * Constants.ballRadius * 2.f)
                  velocity = 0.f @@ 0.f; id = id } ) balls
          pockets = [{ position = 0 @@ 0; hflip = false; vflip = false; sprite = CornerPocket }
                     { position = 0.5 @@ 0; hflip = false; vflip = false; sprite = SidePocketV }
                     { position = 1 @@ 0; hflip = true; vflip = false; sprite = CornerPocket }
                     { position = 1 @@ 0.5; hflip = true; vflip = false; sprite = SidePocketH }
                     { position = 1 @@ 1; hflip = true; vflip = true; sprite = CornerPocket }
                     { position = 0.5 @@ 1; hflip = false; vflip = true; sprite = SidePocketV }
                     { position = 0 @@ 1; hflip = false; vflip = true; sprite = CornerPocket }
                     { position = 0 @@ 0.5; hflip = false; vflip = false; sprite = SidePocketH }]
                     |> List.map (fun pocket -> { pocket with position = pocket.position * (Constants.surfaceLength @@ Constants.surfaceWidth) }) }
    
    let update timeDelta (keyboard: KeyboardState) (mouse: MouseState) game =
        let state, balls =
            match game.state with
            | Aiming(direction) ->
                let cueBall = game.balls |> List.find (fun balls -> balls.id = BallCue)
                let directionVector = ((mouse.X @@ mouse.Y) - (Constants.world2ScreenCoords cueBall.position)) * (1 @@ -1)
                // eew... WHY ARE VECTORS MUTABLE??!??!
                directionVector.Normalize ()
                let direction' = atan2 directionVector.Y directionVector.X
                if keyboard.IsKeyDown Keys.Space || mouse.LeftButton = ButtonState.Pressed then
                    let balls =
                        [for ball in game.balls ->
                            if ball.id = BallCue then { ball with velocity = (cos direction' @@ sin direction') * 4.f}
                            else ball]
                    Settling, balls
                else Aiming(direction'), game.balls
            | Settling ->
                if (game.balls |> List.map (fun ball -> ball.velocity.Length ()) |> List.max) < Constants.minVelocity
                then Aiming(0.f), [for ball in game.balls -> { ball with velocity = 0 @@ 0 }]
                else Settling, game.balls
        { game with
            state = state
            balls = Ball.updateAll timeDelta game.pockets game.walls balls }