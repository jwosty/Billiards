module Billiards.HelperFunctions
open Microsoft.Xna.Framework
open System

let inline (@@) x y = new Vector2(float32 x, float32 y)
let floorv2 (v: Vector2) = new Vector2(floor v.X, floor v.Y)
let roundv2 (v: Vector2) = new Vector2(round v.X, round v.Y)
let normalize (v: Vector2) = v / v.Length ()

let pi = float32 Math.PI

type Vector2 with
    member this.Direction () = atan2 this.Y this.X
    member this.Normalized () =
        let len = this.Length ()
        this.X / len @@ this.Y / len