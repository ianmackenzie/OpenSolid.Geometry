[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Point2d

open OpenSolid.Vector2d

type Point2d with
    static member coordinates (Point2d coordinates_) =
        coordinates_

    static member xCoordinate (Point2d (x, _)) =
        x

    static member yCoordinate (Point2d (_, y)) =
        y

    static member origin =
        Point2d (0.0, 0.0)

    static member polar (r, theta) =
        Point2d (r * cos theta, r * sin theta)

    static member interpolateFrom firstPoint secondPoint parameter =
        let (x1, y1) = Point2d.coordinates firstPoint
        let (x2, y2) = Point2d.coordinates secondPoint
        let x = Scalar.interpolateFrom x1 x2 parameter
        let y = Scalar.interpolateFrom y1 y2 parameter
        Point2d (x, y)

    static member midpoint first second =
        Point2d.interpolateFrom first second 0.5

    static member squaredDistanceFrom firstPoint secondPoint =
        let (x1, y1) = Point2d.coordinates firstPoint
        let (x2, y2) = Point2d.coordinates secondPoint
        let dx = x2 - x1
        let dy = y2 - y1
        dx * dx + dy * dy

    static member distanceFrom firstPoint secondPoint =
        sqrt (Point2d.squaredDistanceFrom firstPoint secondPoint)

    static member vectorFrom (firstPoint : Point2d) (secondPoint : Point2d) =
        secondPoint - firstPoint

    static member equalWithin tolerance firstPoint secondPoint =
        let squaredDistance = Point2d.squaredDistanceFrom firstPoint secondPoint
        squaredDistance <= tolerance * tolerance

    static member directionFrom firstPoint secondPoint =
        Vector2d.direction (Point2d.vectorFrom firstPoint secondPoint)

    static member scaleAbout centerPoint scale point =
        let (x0, y0) = Point2d.coordinates centerPoint
        let (x, y) = Point2d.coordinates point
        Point2d (x0 + scale * (x - x0), y0 + scale * (y - y0))


//  rotateAround
//  translateBy
//  mirrorAcross
//  projectOnto
//  relativeTo
//  placeIn
//  placeOnto
//  hull
