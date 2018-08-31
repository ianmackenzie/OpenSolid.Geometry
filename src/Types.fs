namespace OpenSolid

[<Struct>]
type Vector2d =
    internal Vector2d of (float * float)

[<Struct>]
type Direction2d =
    internal Direction2d of (float * float)

[<Struct>]
type Point2d =
    internal Point2d of (float * float)

type Axis2d =
    internal Axis2d of originPoint : Point2d * direction : Direction2d

type Frame2d =
    internal Frame2d of originPoint : Point2d * xDirection : Direction2d * yDirection : Direction2d
