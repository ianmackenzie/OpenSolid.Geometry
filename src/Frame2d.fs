[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Frame2d

open OpenSolid.Point2d
open OpenSolid.Direction2d

type Frame2d with
    static member originPoint (frame : Frame2d) =
        frame._originPoint

    static member xDirection (frame : Frame2d) =
        frame._xDirection

    static member yDirection (frame : Frame2d) =
        frame._yDirection

    static member xy =
        Frame2d.at Point2d.origin

    static member at point =
        Frame2d (point, Direction2d.x, Direction2d.y)

    static member xAxis frame =
        Axis2d (Frame2d.originPoint frame, Frame2d.xDirection frame)

    static member yAxis frame =
        Axis2d (Frame2d.originPoint frame, Frame2d.yDirection frame)

    static member isRightHanded frame =
        let (x1, y1) = Direction2d.components (Frame2d.xDirection frame)
        let (x2, y2) = Direction2d.components (Frame2d.yDirection frame)
        x1 * y2 - y1 * x2 >= 0.0

    static member flipX frame =
        let originPoint = Frame2d.originPoint frame
        let xDirection = Direction2d.flip (Frame2d.xDirection frame)
        let yDirection = Frame2d.yDirection frame
        Frame2d (originPoint, xDirection, yDirection)

    static member flipY frame =
        let originPoint = Frame2d.originPoint frame
        let xDirection = Frame2d.xDirection frame
        let yDirection = Direction2d.flip (Frame2d.yDirection frame)
        Frame2d (originPoint, xDirection, yDirection)

    static member moveTo point frame =
        let xDirection = Frame2d.xDirection frame
        let yDirection = Frame2d.yDirection frame
        Frame2d(point, xDirection, yDirection)

    static member rotateBy angle =
        let rotateDirection = Direction2d.rotateBy angle
        fun frame ->
            let originPoint = Frame2d.originPoint frame
            let xDirection = rotateDirection (Frame2d.xDirection frame)
            let yDirection = rotateDirection (Frame2d.yDirection frame)
            Frame2d (originPoint, xDirection, yDirection)


type Point2d with
    static member in_ frame coordinates =
        let (x, y) = coordinates
        let (x0, y0) = Point2d.coordinates (Frame2d.originPoint frame)
        let (ux, uy) = Direction2d.components (Frame2d.xDirection frame)
        let (vx, vy) = Direction2d.components (Frame2d.yDirection frame)
        Point2d(x0 + x * ux + y * vx, y0 + x * uy + y * vy)

// rotateAround
// translateBy
// translateAlongOwn
// mirrorAcross
// relativeTo
// placeIn
