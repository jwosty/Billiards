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

/// Projects a vector p perpendicularly onto a line segment given by v1 and v2
let projectToLineSegment (v1: Vector2, v2, p) =
    let l2 = (v1 - v2).LengthSquared ()
    if v1 = v2 then v1
    else
        // Get distance along the line segment onto which p projects
        let t = max 0.f (min 1.f (Vector2.Dot (p - v1, v2 - v1) / l2))
        v1 + t * (v2 - v1)