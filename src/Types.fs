namespace OpenSolid

[<Struct>]
type Vector2d =
    Vector2d of (float * float)

type Vector2d with
    static member (~-) (Vector2d (x, y)) =
        Vector2d(-x, -y)

    static member (+) ((Vector2d (x1, y1)), (Vector2d (x2, y2))) =
        Vector2d(x1 + x2, y1 + y2)

    static member (-) ((Vector2d (x1, y1)), (Vector2d (x2, y2))) =
        Vector2d(x1 - x2, y1 - y2)

    static member (*) (scale, (Vector2d (x, y))) =
        Vector2d(scale * x, scale * y)

    static member (*) ((Vector2d (x, y)), scale) =
        Vector2d(x * scale, y * scale)

    static member (/) ((Vector2d (x, y)), scale) =
        Vector2d(x / scale, y / scale)

[<Struct>]
type Direction2d =
    Direction2d of (float * float)

type Direction2d with
    static member (~-) (Direction2d (x, y)) =
        Direction2d (-x, -y)

    static member (*) (scale, (Direction2d (x, y))) =
        Vector2d(scale * x, scale * y)

    static member (*) ((Direction2d (x, y)), scale) =
        Vector2d(x * scale, y * scale)

[<Struct>]
type Point2d =
    Point2d of (float * float)

type Point2d with
    static member (+) ((Point2d (px, py)), (Vector2d (vx, vy))) =
        Point2d(px + vx, py + vy)

    static member (-) ((Point2d (px, py)), (Vector2d (vx, vy))) =
        Point2d(px - vx, py - vy)

    static member (-) ((Point2d (x1, y1)), (Point2d (x2, y2))) =
        Vector2d(x1 - x2, y1 - y2)

type Axis2d(originPoint : Point2d, direction : Direction2d) =
    member inline internal this._originPoint =
        originPoint

    member inline internal this._direction =
        direction

type Frame2d(originPoint : Point2d, xDirection : Direction2d, yDirection : Direction2d) =
    member inline internal this._originPoint =
        originPoint

    member inline internal this._xDirection =
        xDirection

    member inline internal this._yDirection =
        yDirection

