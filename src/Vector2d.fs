[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Vector2d

let xComponent (Vector2d(x, _)) =
    x

let yComponent (Vector2d(_, y)) =
    y

let components (Vector2d(x, y)) =
    (x, y)

let zero =
    Vector2d(0.0, 0.0)

let polar (r, theta) =
    Vector2d(r * cos theta, r * sin theta)

let perpendicularTo (Vector2d(x, y)) =
    Vector2d(-y, x)

let interpolateFrom (Vector2d(x1, y1)) (Vector2d(x2, y2)) parameter =
    let x = Scalar.interpolateFrom x1 x2 parameter
    let y = Scalar.interpolateFrom y1 y2 parameter
    Vector2d(x, y)

let sum (Vector2d(x1, y1)) (Vector2d(x2, y2)) =
    Vector2d(x1 + x2, y1 + y2)

let difference (Vector2d(x1, y1)) (Vector2d(x2, y2)) =
    Vector2d(x1 - x2, y1 - y2)

let squaredLength (Vector2d(x, y)) =
    x * x + y * y

let length vector =
    sqrt (squaredLength vector)

let equalWithin tolerance firstVector secondVector =
    squaredLength (difference firstVector secondVector) <= tolerance * tolerance

let dotProduct (Vector2d(x1, y1)) (Vector2d(x2, y2)) =
    x1 * x2 + y1 * y2

let crossProduct (Vector2d(x1, y1)) (Vector2d(x2, y2)) =
    x1 * y2 - y1 * x2

let componentIn (Direction2d(dx, dy)) (Vector2d(vx, vy)) =
    vx * dx + vy * dy

let flip (Vector2d(x, y)) =
    Vector2d(-x, -y)

let scaleBy scale (Vector2d(x, y)) =
    Vector2d(x * scale, y * scale)

let rotateBy angle =
    let cosine = cos angle
    let sine = sin angle
    fun (Vector2d(x, y)) ->
        Vector2d(x * cosine - y * sine, x * sine + y * cosine)

let in_ (Direction2d(dx, dy)) length =
    Vector2d(length * dx, length * dy)

let direction (Vector2d(x, y) as vector) =
    let vectorLength = length vector
    if vectorLength > 0.0 then
        let dx = x / vectorLength
        let dy = y / vectorLength
        Some(Direction2d(dx, dy))
    else
        None

let lengthAndDirection (Vector2d(x, y) as vector) =
    let vectorLength = length vector
    if vectorLength > 0.0 then
        let dx = x / vectorLength
        let dy = y / vectorLength
        Some((length, Direction2d(dx, dy)))
    else
        None

let projectionIn direction vector =
    in_ direction (componentIn direction vector)

// mirrorAcross
// projectOnto
// relativeTo
// placeIn
// placeOnto
