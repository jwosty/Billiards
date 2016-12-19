namespace Billiards
open Billiards.HelperFunctions
open Microsoft.FSharp.Reflection
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.Diagnostics

type GameWindow() as this =
    inherit Game()
    
    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = null
    let mutable pixelSprite = null
    let mutable textures: Map<Sprite, Texture2D> = Map.empty
    
    let mutable game = Game.init ()
    let framerateTimer = new Stopwatch()
    let mutable skipUpdate = true
    let mutable hasStepped = false
    
    do
        this.IsMouseVisible <- true
        this.Content.RootDirectory <- "Assets"
        let ww, wh = int (Constants.tableWidth * Constants.scale), int (Constants.tableLength * Constants.scale)
        graphics.PreferredBackBufferWidth <- wh
        graphics.PreferredBackBufferHeight <- ww
    
    override this.LoadContent () =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        pixelSprite <- new Texture2D(this.GraphicsDevice, 1, 1)
        pixelSprite.SetData [|Color.White|]
        textures <-
            Map.ofList
                [for caseInfo in FSharpType.GetUnionCases typeof<Sprite> do
                    let t = typeof<Sprite>
                    if (caseInfo.GetFields ()).Length = 0 then
                        let case = FSharpValue.MakeUnion (caseInfo, [||]) :?> Sprite
                        yield case, this.Content.Load (caseInfo.Name.ToLower ())
                    else
                        for caseInfo' in FSharpType.GetUnionCases typeof<BallId> do
                             let ballIdCase = FSharpValue.MakeUnion (caseInfo', [||]) :?> BallId
                             let spriteCase = FSharpValue.MakeUnion (caseInfo, [|ballIdCase :> obj|]) :?> Sprite
                             yield spriteCase, this.Content.Load (caseInfo'.Name.ToLower ())]
    
    override this.Update gameTime =
        let keyboard = Keyboard.GetState ()
        if keyboard.IsKeyDown Keys.Escape then
            this.Exit ()
        let shouldStep =
            if keyboard.IsKeyDown Keys.Z then
                if not hasStepped then
                    hasStepped <- true
                    true
                else false
            else
                hasStepped <- false
                false
        let shouldStep = shouldStep || keyboard.IsKeyDown Keys.X
        
        //let shouldStep = true
        if shouldStep then
            game <- Game.update (float32 gameTime.ElapsedGameTime.TotalSeconds) keyboard (Mouse.GetState ()) game
    
    member this.DrawBall ball =
        let screenPosition = Constants.world2ScreenCoords (ball.position + (-Constants.ballRadius @@ Constants.ballRadius))
        spriteBatch.Draw (textures.[Sprite.Ball(ball.id)], roundv2 screenPosition, Color.White)
    
    override this.Draw gameTime =
        this.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin ()
        spriteBatch.Draw (textures.[Table], 0 @@ 0, Color.White)
        for pocket in game.pockets do
            let tex = textures.[pocket.sprite]
            let fx = (if pocket.hflip then SpriteEffects.FlipHorizontally else SpriteEffects.None) ||| (if pocket.vflip then SpriteEffects.FlipVertically else SpriteEffects.None)
            spriteBatch.Draw (tex, roundv2 (Constants.world2ScreenCoords pocket.position), new Nullable<_>(), Color.White, 0.f, (tex.Width / 2 @@ tex.Height / 2), 1 @@ 1, fx, 0.f)
            //if pocket.sprite = SidePocketV && pocket.vflip then
            //    spriteBatch.Draw (pixelSprite, Constants.world2ScreenCoords (pocket.position - (Constants.pocketWidth / 2.f @@ 0)), new Nullable<_>(), Color.White, 0.f, 0 @@ 0, (Constants.pocketWidth * Constants.scale) @@ 1, SpriteEffects.None, 0.f)
        match game.state, List.tryFind (fun ball -> ball.id = BallCue) game.balls with
        | Aiming(direction), Some(cueBall) ->
            spriteBatch.Draw (pixelSprite, Constants.world2ScreenCoords cueBall.position, new Nullable<_>(), Color.Blue, -direction, 0 @@ 0, 50 @@ 1, SpriteEffects.None, 0.f)
        | _ -> ()
        game.balls |> List.iter this.DrawBall
        spriteBatch.End ()
    
module Main =
    [<EntryPoint>]
    let main args =
        using (new GameWindow()) (fun gw -> gw.Run ())
        0