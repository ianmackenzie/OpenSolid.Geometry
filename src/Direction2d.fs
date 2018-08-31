[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Direction2d

let unsafe_ components =
    Direction2d components

let components (Direction2d components) =
    components

let from firstPoint secondPoint =
    Vector2d.direction (Vector2d.from firstPoint secondPoint)

let xComponent (Direction2d ( x, _ )) =
    x

let yComponent  (Direction2d ( _, y )) =
    y

let toVector direction =
    Vector2d.fromComponents (components direction)

let componentIn firstDirection secondDirection =
    let ( x1, y1 ) = components firstDirection
    let ( x2, y2 ) = components secondDirection
    x1 * x2 + y1 * y2

let x =
    Direction2d ( 1.0, 0.0 )

let y =
    Direction2d ( 0.0, 1.0 )

let positiveX =
    Direction2d ( 1.0, 0.0 )

let positiveY =
    Direction2d ( 0.0, 1.0 )

let negativeX =
    Direction2d ( -1.0, 0.0 )

let negativeY =
    Direction2d ( 0.0, -1.0 )

let fromAngle theta =
    Direction2d ( cos theta, sin theta )

let toAngle direction =
    let ( x, y ) = components direction
    atan2 y x

let perpendicularTo direction =
    let ( x, y ) = components direction
    Direction2d ( -y, x )

let angleFrom other direction =
    let otherVector = toVector other
    let directionVector = toVector direction
    let x = Vector2d.dotProduct otherVector directionVector
    let y = Vector2d.crossProduct otherVector directionVector
    atan2 y x

let equalWithin tolerance firstDirection secondDirection =
    abs (angleFrom firstDirection secondDirection) <= tolerance

let flip direction =
    let ( x, y ) = components direction
    Direction2d ( -x, -y )

let rotateCounterclockwise direction =
    let ( x, y ) = components direction
    Direction2d ( -y, x )

let rotateClockwise direction =
    let ( x, y ) = components direction
    Direction2d ( y, -x )

let rotateBy angle =
    let cosine = cos angle
    let sine = sin angle
    fun direction ->
        let ( x, y ) = components direction
        Direction2d ( x * cosine - y * sine, x * sine + y * cosine )

// mirrorAcross
// relativeTo
// placeIn
// placeOnto
