[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Vector2d

[<Struct>]
type Vector2d =
    Vector2d of (float * float)

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
        let (x, y) = Vector2d.components vector
        Vector2d (-y, x)

    static member (~-) vector =
        let (x, y) = Vector2d.components vector
        Vector2d(-x, -y)

    static member (+) (firstVector, secondVector) =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        Vector2d (x1 + x2, y1 + y2)

    static member (-) (firstVector, secondVector) =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        Vector2d (x1 - x2, y1 - y2)

    static member (*) (scale, vector) =
        let (x, y) = Vector2d.components vector
        Vector2d (scale * x, scale * y)

    static member (*) (vector, scale) =
        let (x, y) = Vector2d.components vector
        Vector2d (x * scale, y * scale)

    static member (/) (vector, scale) =
        let (x, y) = Vector2d.components vector
        Vector2d (x / scale, y / scale)

    static member interpolateFrom firstVector secondVector parameter =
        let (x1, y1) = Vector2d.components firstVector
        let (x2, y2) = Vector2d.components secondVector
        let x = Scalar.interpolateFrom x1 x2 parameter
        let y = Scalar.interpolateFrom y1 y2 parameter
        Vector2d (x, y)

    static member equalWithin tolerance firstVector secondVector =
        Vector2d.squaredLength (secondVector - firstVector) <= tolerance * tolerance

    static member squaredLength vector =
        let (x, y) = Vector2d.components vector
        x * x + y * y

    static member length vector =
        sqrt (Vector2d.squaredLength vector)

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
