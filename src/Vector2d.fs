[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Vector2d

type Vector2d with
    static member components (Vector2d components_) =
        components_

    static member xComponent (Vector2d (x, _)) =
        x

    static member yComponent (Vector2d (_, y)) =
        y

    static member zero =
        Vector2d (0.0, 0.0)

    static member polar (r, theta) =
        Vector2d (r * cos theta, r * sin theta)

    static member perpendicularTo vector =
        Vector2d (-Vector2d.yComponent vector, Vector2d.xComponent vector)

    static member interpolateFrom firstVector secondVector parameter =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        let x = Scalar.interpolateFrom x1 x2 parameter
        let y = Scalar.interpolateFrom y1 y2 parameter
        Vector2d (x, y)

    static member equalWithin tolerance firstVector secondVector =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        let dx = x2 - x1
        let dy = y2 - y1
        dx * dx + dy * dy <= tolerance * tolerance

    static member squaredLength vector =
        let (x, y) = Vector2d.components vector
        x * x + y * y

    static member length vector =
        sqrt (Vector2d.squaredLength vector)

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

    static member dotProduct firstVector secondVector =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        x1 * x2 + y1 * y2

    static member crossProduct firstVector secondVector =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        x1 * y2 - x2 * y1

    static member flip (vector : Vector2d) =
        -vector

    static member scaleBy scale (vector : Vector2d) =
        vector * scale

    static member rotateBy angle =
        let cosine = cos angle
        let sine = sin angle
        fun vector ->
            let (x, y) = Vector2d.components vector
            Vector2d (x * cosine - y * sine, x * sine + y * cosine)


// mirrorAcross
// projectionIn
// projectOnto
// relativeTo
// placeIn
// placeOnto
