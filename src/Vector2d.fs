[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Vector2d

let components (Vector2d components) =
    components

let xComponent vector =
    let (x, _) = components vector
    x

let yComponent vector =
    let (_, y) = components vector
    y

let zero =
    Vector2d (0.0, 0.0)

let polar (r, theta) =
    Vector2d (r * cos theta, r * sin theta)

let perpendicularTo vector =
    let (x, y) = components vector
    Vector2d (-y, x)

let interpolateFrom firstVector secondVector parameter =
    let (x1, y1) = components firstVector
    let (x2, y2) = components secondVector
    let x = Scalar.interpolateFrom x1 x2 parameter
    let y = Scalar.interpolateFrom y1 y2 parameter
    Vector2d (x, y)

let sum firstVector secondVector =
    let (x1, y1) = components firstVector
    let (x2, y2) = components secondVector
    Vector2d (x1 + x2, y1 + y2)

let difference firstVector secondVector =
    let (x1, y1) = components firstVector
    let (x2, y2) = components secondVector
    Vector2d(x1 - x2, y1 - y2)

let squaredLength vector =
    let (x, y) = components vector
    x * x + y * y

let length vector =
    sqrt (squaredLength vector)

let equalWithin tolerance firstVector secondVector =
    squaredLength (difference firstVector secondVector) <= tolerance * tolerance

let dotProduct firstVector secondVector =
    let (x1, y1) = components firstVector
    let (x2, y2) = components secondVector
    x1 * x2 + y1 * y2

let crossProduct firstVector secondVector =
    let (x1, y1) = components firstVector
    let (x2, y2) = components secondVector
    x1 * y2 - y1 * x2

let componentIn (Direction2d (dx, dy)) vector =
    let (vx, vy) = components vector
    vx * dx + vy * dy

let flip vector =
    let (x, y) = components vector
    Vector2d (-x, -y)

let scaleBy scale vector =
    let (x, y) = components vector
    Vector2d (x * scale, y * scale)

let rotateBy angle =
    let cosine = cos angle
    let sine = sin angle
    fun vector ->
        let (x, y) = components vector
        Vector2d (x * cosine - y * sine, x * sine + y * cosine)

let in_ (Direction2d (dx, dy)) length =
    Vector2d (length * dx, length * dy)

let direction vector =
    let vectorLength = length vector
    if vectorLength > 0.0 then
        let (x, y) = components vector
        let dx = x / vectorLength
        let dy = y / vectorLength
        Some (Direction2d (dx, dy))
    else
        None

let lengthAndDirection vector =
    let vectorLength = length vector
    if vectorLength > 0.0 then
        let (x, y) = components vector
        let dx = x / vectorLength
        let dy = y / vectorLength
        Some (length, Direction2d (dx, dy))
    else
        None

let projectionIn direction vector =
    in_ direction (componentIn direction vector)

// mirrorAcross
// projectOnto
// relativeTo
// placeIn
// placeOnto
