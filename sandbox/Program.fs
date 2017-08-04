open System

open Chiron

open OpenSolid
open OpenSolid.Vector2d

[<EntryPoint>]
let main argv =
    let vector = Vector2d(2.0, 3.0)
    printfn "%s" (Json.format (Json.serialize vector))
    let json = """{"originPoint": {"x": 3, "y": 4}, "direction": {"x": 1, "yz": 0}}"""
    let deserialized : Choice<Axis2d, string> = Json.tryDeserialize (Json.parse json)
    printfn "%A" deserialized
    0
