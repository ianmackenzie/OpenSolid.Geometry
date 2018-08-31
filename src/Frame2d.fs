[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Frame2d

let unsafe_ originPoint xDirection yDirection =
    Frame2d
        ( originPoint = originPoint,
          xDirection = xDirection,
          yDirection = yDirection )

let withXDirection xDirection originPoint =
    Frame2d
        ( originPoint = originPoint,
          xDirection = xDirection,
          yDirection = Direction2d.rotateCounterclockwise xDirection )

let withYDirection yDirection originPoint =
    Frame2d
        ( originPoint = originPoint,
          xDirection = Direction2d.rotateClockwise yDirection,
          yDirection = yDirection )

let originPoint (Frame2d ( originPoint = originPoint )) =
    originPoint

let xDirection (Frame2d ( xDirection = xDirection )) =
    xDirection

let yDirection (Frame2d ( yDirection = yDirection )) =
    yDirection

let atPoint point =
    Frame2d
        ( originPoint = point,
          xDirection = Direction2d.x,
          yDirection = Direction2d.y )

let xy =
    atPoint Point2d.origin

let xAxis frame =
    Axis2d.through (originPoint frame) (xDirection frame)

let yAxis frame =
    Axis2d.through (originPoint frame) (yDirection frame)

let isRightHanded frame =
    let xDirectionVector = Direction2d.toVector (xDirection frame)
    let yDirectionVector = Direction2d.toVector (yDirection frame)
    Vector2d.crossProduct xDirectionVector yDirectionVector >= 0.0

let flipX frame =
    Frame2d
        ( originPoint = originPoint frame,
          xDirection = Direction2d.flip (xDirection frame),
          yDirection = yDirection frame )

let flipY frame =
    Frame2d
        ( originPoint = originPoint frame,
          xDirection = xDirection frame,
          yDirection = Direction2d.flip (yDirection frame) )

let moveTo point frame =
    Frame2d
        ( originPoint = point,
          xDirection = xDirection frame,
          yDirection = yDirection frame )

let rotateBy angle =
    let rotateDirection = Direction2d.rotateBy angle
    fun frame ->
        Frame2d
            ( originPoint = originPoint frame,
              xDirection = rotateDirection (xDirection frame),
              yDirection = rotateDirection (yDirection frame) )

// rotateAround
// translateBy
// translateAlongOwn
// mirrorAcross
// relativeTo
// placeIn
