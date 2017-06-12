namespace OpenSolid

type Components2d =
    float * float

type Coordinates2d =
    float * float

[<Struct>]
type Vector2d =
    Vector2d of Components2d

[<Struct>]
type Direction2d =
    Direction2d of Components2d

[<Struct>]
type Point2d =
    Point2d of Coordinates2d

type Axis2d =
    Axis2d of originPoint : Point2d * direction : Direction2d

type Frame2d =
    Frame2d of originPoint : Point2d * xDirection : Direction2d * yDirection : Direction2d
