namespace OpenSolid

open Chiron

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

type Vector2d with
    static member ToJson (Vector2d (x, y)) = json {
        do! Json.write "x" x
        do! Json.write "y" y
    }

    static member FromJson (_: Vector2d) = json {
        let! x = Json.read "x"
        let! y = Json.read "y"
        return Vector2d (x, y)
    }

type Direction2d with
    static member ToJson (Direction2d (x, y)) = json {
        do! Json.write "x" x
        do! Json.write "y" y
    }

    static member FromJson (_: Direction2d) = json {
        let! x = Json.read "x"
        let! y = Json.read "y"
        return Direction2d (x, y)
    }

type Point2d with
    static member ToJson (Point2d (x, y)) = json {
        do! Json.write "x" x
        do! Json.write "y" y
    }

    static member FromJson (_: Point2d) = json {
        let! x = Json.read "x"
        let! y = Json.read "y"
        return Point2d (x, y)
    }

type Axis2d with
    static member ToJson (Axis2d(originPoint, direction)) = json {
        do! Json.write "originPoint" originPoint
        do! Json.write "direction" direction
    }

    static member FromJson (_: Axis2d) = json {
        let! originPoint = Json.read "originPoint"
        let! direction = Json.read "direction"
        return Axis2d(originPoint, direction)
    }
