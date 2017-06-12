[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Frame2d

let originPoint (Frame2d(originPoint, _, _)) =
    originPoint

let xDirection (Frame2d(_, xDirection, _)) =
    xDirection

let yDirection (Frame2d(_, _, yDirection)) =
    yDirection

let at point =
    Frame2d(
        originPoint = point,
        xDirection = Direction2d.x,
        yDirection = Direction2d.y
    )

let xy =
    at Point2d.origin

let xAxis frame =
    Axis2d(
        originPoint = originPoint frame,
        direction = xDirection frame
    )

let yAxis frame =
    Axis2d(
        originPoint = originPoint frame,
        direction = yDirection frame
    )

let isRightHanded frame =
    let xDirectionVector = Direction2d.toVector (xDirection frame)
    let yDirectionVector = Direction2d.toVector (yDirection frame)
    Vector2d.crossProduct xDirectionVector yDirectionVector >= 0.0

let flipX frame =
    Frame2d(
        originPoint = originPoint frame,
        xDirection = Direction2d.flip (xDirection frame),
        yDirection = yDirection frame
    )

let flipY frame =
    Frame2d(
        originPoint = originPoint frame,
        xDirection = xDirection frame,
        yDirection = Direction2d.flip (yDirection frame)
    )

let moveTo point frame =
    Frame2d(
        originPoint = point,
        xDirection = xDirection frame,
        yDirection = yDirection frame
    )

let rotateBy angle =
    let rotateDirection = Direction2d.rotateBy angle
    fun frame ->
        Frame2d(
            originPoint = originPoint frame,
            xDirection = rotateDirection (xDirection frame),
            yDirection = rotateDirection (yDirection frame)
        )

// rotateAround
// translateBy
// translateAlongOwn
// mirrorAcross
// relativeTo
// placeIn
