[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Direction2d

let inline xComponent (direction: Direction2d) =
    direction.XComponent

let inline yComponent (direction: Direction2d) =
    direction.YComponent

let inline components (direction: Direction2d) =
    (direction.XComponent, direction.YComponent)

let toVector direction =
    Vector2d (components direction)

let componentIn firstDirection secondDirection =
    let (x1, y1) = components firstDirection
    let (x2, y2) = components secondDirection
    x1 * x2 + y1 * y2

let x =
    Direction2d (1.0, 0.0)

let y =
    Direction2d (0.0, 1.0)

let fromAngle theta =
    Direction2d (cos theta, sin theta)

let toAngle direction =
    let (x, y) = components direction
    atan2 y x

let perpendicularTo direction =
    let (x, y) = components direction
    Direction2d (-y, x)

let angleFrom other direction =
    let otherVector = toVector other
    let directionVector = toVector direction
    let x = Vector2d.dotProduct otherVector directionVector
    let y = Vector2d.crossProduct otherVector directionVector
    atan2 y x

let equalWithin tolerance firstDirection secondDirection =
    abs (angleFrom firstDirection secondDirection) <= tolerance

let flip direction =
    let (x, y) = components direction
    Direction2d (-x, -y)

let rotateBy angle =
    let cosine = cos angle
    let sine = sin angle
    fun direction ->
        let (x, y) = components direction
        Direction2d (x * cosine - y * sine, x * sine + y * cosine)

// mirrorAcross
// relativeTo
// placeIn
// placeOnto
