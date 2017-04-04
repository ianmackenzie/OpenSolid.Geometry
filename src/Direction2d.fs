module OpenSolid.Direction2d

open OpenSolid.Vector2d

[<Struct>]
type Direction2d =
    Direction2d of (float * float)

type Direction2d with
    static member components (Direction2d components_) =
        components_

    static member xComponent (Direction2d (x, _)) =
        x

    static member yComponent (Direction2d (_, y)) =
        y

    static member toVector direction =
        Vector2d (Direction2d.components direction)

    static member componentIn firstDirection secondDirection =
        let firstVector = Direction2d.toVector firstDirection
        let secondVector = Direction2d.toVector secondDirection
        Vector2d.dotProduct firstVector secondVector

    static member x =
        Direction2d (1.0, 0.0)

    static member y =
        Direction2d (0.0, 1.0)

    static member fromAngle theta =
        Direction2d (cos theta, sin theta)

    static member toAngle direction =
        let (x, y) = Direction2d.components direction
        atan2 y x

    static member perpendicularTo direction =
        let (x, y) = Direction2d.components direction
        Direction2d (-y, x)

    static member (~-) direction =
        let (x, y) = Direction2d.components direction
        Direction2d (-x, -y)

    static member (*) (scale, direction) =
        let (x, y) = Direction2d.components direction
        Vector2d (scale * x, scale * y)

    static member (*) (direction, scale) =
        let (x, y) = Direction2d.components direction
        Vector2d (x * scale, y * scale)

    static member angleFrom other direction =
        let otherVector = Direction2d.toVector other
        let directionVector = Direction2d.toVector direction
        let x = Vector2d.dotProduct otherVector directionVector
        let y = Vector2d.crossProduct otherVector directionVector
        atan2 y x

    static member equalWithin tolerance firstDirection secondDirection =
        abs (Direction2d.angleFrom firstDirection secondDirection) <= tolerance

    static member flip (direction : Direction2d) =
        -direction

    static member rotateBy angle =
        let cosine = cos angle
        let sine = sin angle
        fun direction ->
            let (x, y) = Direction2d.components direction
            Direction2d (x * cosine - y * sine, x * sine + y * cosine)

type Vector2d with
    static member in_ direction length =
        length * direction

    static member componentIn direction vector =
        Vector2d.dotProduct vector (Direction2d.toVector direction)

    static member direction vector =
        let length = Vector2d.length vector
        if length > 0.0 then
            let (x, y) = Vector2d.components vector
            Some (Direction2d (x / length, y / length))
        else
            None

    static member lengthAndDirection vector =
        let length = Vector2d.length vector
        if length > 0.0 then
            let (x, y) = Vector2d.components vector
            Some (length, Direction2d (x / length, y / length))
        else
            None

    static member projectionIn direction vector =
        Vector2d.in_ direction (Vector2d.componentIn direction vector)



// mirrorAcross
// relativeTo
// placeIn
// placeOnto
