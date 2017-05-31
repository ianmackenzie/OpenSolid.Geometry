[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Frame2d

let originPoint (Frame2d(originPoint, _, _)) =
    originPoint

let xDirection (Frame2d(_, xDirection, _)) =
    xDirection

let yDirection (Frame2d(_, _, yDirection)) =
    yDirection

let at point =
    Frame2d(point, Direction2d.x, Direction2d.y)

let xy =
    at Point2d.origin

let xAxis frame =
    Axis2d(originPoint frame, xDirection frame)

let yAxis frame =
    Axis2d(originPoint frame, yDirection frame)

let isRightHanded frame =
    let xDirectionVector = Direction2d.toVector (xDirection frame)
    let yDirectionVector = Direction2d.toVector (yDirection frame)
    Vector2d.crossProduct xDirectionVector yDirectionVector >= 0.0

let flipX frame =
    let originPoint = originPoint frame
    let xDirection = xDirection frame
    let yDirection = yDirection frame
    Frame2d(originPoint, Direction2d.flip xDirection, yDirection)

let flipY frame =
    let originPoint = originPoint frame
    let xDirection = xDirection frame
    let yDirection = yDirection frame
    Frame2d(originPoint, xDirection, Direction2d.flip yDirection)

let moveTo point frame =
    let xDirection = xDirection frame
    let yDirection = yDirection frame
    Frame2d(point, xDirection, yDirection)

let rotateBy angle =
    let rotateDirection = Direction2d.rotateBy angle
    fun frame ->
        let originPoint = originPoint frame
        let xDirection = rotateDirection (xDirection frame)
        let yDirection = rotateDirection (yDirection frame)
        Frame2d(originPoint, xDirection, yDirection)

// rotateAround
// translateBy
// translateAlongOwn
// mirrorAcross
// relativeTo
// placeIn
