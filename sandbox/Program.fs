open System

open OpenSolid

let degrees degrees_ =
    degrees_ * Math.PI / 180.0

[<EntryPoint>]
let main argv =
    let vector = Vector2d.fromComponents ( 2.0, 3.0 )
    printfn "Vector length: %f" (Vector2d.length vector)
    let frame =
        Frame2d.fromOriginAndXDirection
            { originPoint = Point2d.fromCoordinates ( 1.0, 1.0 )
              xDirection = Direction2d.fromAngle (degrees 30.0) }
    printfn "Frame: %O" frame
    0
