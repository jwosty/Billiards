namespace Billiards
open Billiards.HelperFunctions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open System

type BallId =
    | BallCue | Ball1 | Ball2 | Ball3 | Ball4 | Ball5 | Ball6 | Ball7
    | Ball8 | Ball9 | Ball10 | Ball11 | Ball12 | Ball13 | Ball14 | Ball15

type Ball = { position: Vector2; velocity: Vector2; id: BallId }

type GameState = | Aiming of float32 | Settling

type Game =
    { state: GameState; balls: Ball list }

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
    /// Represents μ in   Ff = μFn
    let ballSurfaceFriction = 0.02f
    let minVelocity = 0.01f
    
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
    
    let move (timeDelta: float32) ball = { ball with position = ball.position + (ball.velocity * timeDelta) }
    
    let update (timeDelta: float32) ball =
        let position_ = ball.position + (ball.velocity * timeDelta)
        
        let velocity', position' =
            // bottom cushion
            if (position_.Y < Constants.ballRadius && ball.velocity.Y < 0.f) then
                let newTimeDelta = (0.f + Constants.ballRadius - ball.position.Y) / ball.velocity.Y
                ball.velocity * (1 @@ -1), (ball.position + (ball.velocity * newTimeDelta))
            // top cushion
            elif (position_.Y > Constants.surfaceWidth - Constants.ballRadius && ball.velocity.Y > 0.f) then
                let newTimeDelta = (Constants.surfaceWidth - Constants.ballRadius - ball.position.Y) / ball.velocity.Y
                ball.velocity * (1 @@ -1), (ball.position + (ball.velocity * newTimeDelta))
            // left cushion
            elif (position_.X - Constants.ballRadius < 0.f && ball.velocity.X < 0.f) then
                let newTimeDelta = (0.f + Constants.ballRadius - ball.position.X) / ball.velocity.X
                ball.velocity * (-1 @@ 1), (ball.position + (ball.velocity * newTimeDelta))
            // right cushion
            elif (position_.X > Constants.surfaceLength - Constants.ballRadius && ball.velocity.X > 0.f) then
                let newTimeDelta = (Constants.surfaceLength - Constants.ballRadius - ball.position.X) / ball.velocity.X
                ball.velocity * (-1 @@ 1), (ball.position + (ball.velocity * newTimeDelta))
            else ball.velocity, position_
        
        { ball with
            position = position'
            velocity = velocity' * (1.f - Constants.ballSurfaceFriction) }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Game =
    let init () =
        { state = Settling
          balls = [{ position = 0.75f @@ 0.75f; velocity = 0 @@ 0; id = BallCue }
                   { position = 1.75f @@ 0.75f; velocity = 0 @@ 0; id = Ball6 }
                   { position = 1.75f @@ (Constants.surfaceWidth - Constants.ballRadius); velocity = 0 @@ 0; id = Ball15 }] }
    
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
                            if ball.id = BallCue then { ball with velocity = (cos direction' @@ sin direction') * 3.f}
                            else ball]
                    Settling, balls
                else Aiming(direction'), game.balls
            | Settling ->
                if (game.balls |> List.map (fun ball -> ball.velocity.Length ()) |> List.max) < Constants.minVelocity
                then Aiming(0.f), [for ball in game.balls -> { ball with velocity = 0 @@ 0 }]
                else Settling, game.balls
        { game with
            state = state
            balls = List.map (Ball.update timeDelta) balls }