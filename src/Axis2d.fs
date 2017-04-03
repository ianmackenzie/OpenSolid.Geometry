[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Axis2d

open OpenSolid.Point2d
open OpenSolid.Direction2d

type Axis2d with
    static member originPoint(axis : Axis2d) =
        axis._originPoint

    static member direction(axis : Axis2d) =
        axis._direction

type Point2d with
    static member along axis distance =
        let (x0, y0) = Point2d.coordinates (Axis2d.originPoint axis)
        let (dx, dy) = Direction2d.components (Axis2d.direction axis)
        Point2d(x0 + distance * dx, y0 + distance * dy)

    static member distanceAlong axis point =
        let (px, py) = Point2d.coordinates point
        let (x0, y0) = Point2d.coordinates (Axis2d.originPoint axis)
        let (dx, dy) = Direction2d.components (Axis2d.direction axis)
        (px - x0) * dx + (py - y0) * dy

    static member signedDistanceFrom axis point =
        let (px, py) = Point2d.coordinates point
        let (x0, y0) = Point2d.coordinates (Axis2d.originPoint axis)
        let (dx, dy) = Direction2d.components (Axis2d.direction axis)
        (py - y0) * dx - (px - x0) * dy



