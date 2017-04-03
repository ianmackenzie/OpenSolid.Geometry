[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Direction2d

open OpenSolid.Vector2d

type Direction2d with
    static member components (Direction2d components_) =
        components_

    static member xComponent (Direction2d (x, _)) =
        x

    static member yComponent (Direction2d (_, y)) =
        y

    static member componentIn firstDirection secondDirection =
        let (x1, y1) = Direction2d.components firstDirection
        let (x2, y2) = Direction2d.components secondDirection
        x1 * x2 + y1 * y2

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

    static member angleFrom other direction =
        let (x0, y0) = Direction2d.components other
        let (x1, y1) = Direction2d.components direction
        let x = x0 * x1 + y0 * y1
        let y = x0 * y1 - y0 * x1
        atan2 y x

    static member equalWithin tolerance firstDirection secondDirection =
        abs (Direction2d.angleFrom firstDirection secondDirection) <= tolerance

    static member toVector direction =
        Vector2d (Direction2d.components direction)

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
        let (x, y) = Direction2d.components direction
        Vector2d (length * x, length * y)

    static member componentIn direction vector =
        let (dx, dy) = Direction2d.components direction
        let (vx, vy) = Vector2d.components vector
        vx * dx + vy * dy



// mirrorAcross
// relativeTo
// placeIn
// placeOnto
