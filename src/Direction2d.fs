[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Direction2d

let xComponent (Direction2d(x, _)) =
    x

let yComponent (Direction2d(_, y)) =
    y

let components (Direction2d(x, y)) =
    (x, y)

let toVector (Direction2d(x, y)) =
    Vector2d(x, y)

let componentIn (Direction2d(x1, y1)) (Direction2d(x2, y2)) =
    x1 * x2 + y1 * y2

let x =
    Direction2d(1.0, 0.0)

let y =
    Direction2d(0.0, 1.0)

let positiveX =
    Direction2d(1.0, 0.0)

let positiveY =
    Direction2d(0.0, 1.0)

let negativeX =
    Direction2d(-1.0, 0.0)

let negativeY =
    Direction2d(0.0, -1.0)

let fromAngle theta =
    Direction2d(cos theta, sin theta)

let toAngle (Direction2d(x, y)) =
    atan2 y x

let perpendicularTo (Direction2d(x, y)) =
    Direction2d(-y, x)

let angleFrom other direction =
    let otherVector = toVector other
    let directionVector = toVector direction
    let x = Vector2d.dotProduct otherVector directionVector
    let y = Vector2d.crossProduct otherVector directionVector
    atan2 y x

let equalWithin tolerance firstDirection secondDirection =
    abs (angleFrom firstDirection secondDirection) <= tolerance

let flip (Direction2d(x, y)) =
    Direction2d(-x, -y)

let rotateBy angle =
    let cosine = cos angle
    let sine = sin angle
    fun (Direction2d(x, y)) ->
        Direction2d(x * cosine - y * sine, x * sine + y * cosine)

// mirrorAcross
// relativeTo
// placeIn
// placeOnto
