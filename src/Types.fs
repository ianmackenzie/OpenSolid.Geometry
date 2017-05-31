namespace OpenSolid

type Vector2d =
    Vector2d of x : float * y : float

type Direction2d =
    Direction2d of x : float * y : float

type Point2d =
    Point2d of x : float * y : float

type Axis2d =
    Axis2d of originPoint : Point2d * direction : Direction2d

type Frame2d =
    Frame2d of originPoint : Point2d * xDirection : Direction2d * yDirection : Direction2d
