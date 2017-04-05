[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Point2d

let xCoordinate (point: Point2d) =
    point.XCoordinate

let yCoordinate (point: Point2d) =
    point.YCoordinate

let coordinates (point: Point2d) =
    (point.XCoordinate, point.YCoordinate)

let origin =
    Point2d (0.0, 0.0)

let polar (r, theta) =
    Point2d (r * cos theta, r * sin theta)

let interpolateFrom firstPoint secondPoint parameter =
    let (x1, y1) = coordinates firstPoint
    let (x2, y2) = coordinates secondPoint
    let x = Scalar.interpolateFrom x1 x2 parameter
    let y = Scalar.interpolateFrom y1 y2 parameter
    Point2d (x, y)

let midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5

let vectorFrom firstPoint secondPoint =
    let (x1, y1) = coordinates firstPoint
    let (x2, y2) = coordinates secondPoint
    Vector2d (x2 - x1, y2 - y1)

let squaredDistanceFrom firstPoint secondPoint =
    Vector2d.squaredLength (vectorFrom firstPoint secondPoint)

let distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)

let equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance

let directionFrom firstPoint secondPoint =
    Vector2d.direction (vectorFrom firstPoint secondPoint)

let translateBy displacement point =
    let (dx, dy) = Vector2d.components displacement
    let (px, py) = coordinates point
    Point2d (px + dx, py + dy)

let scaleAbout centerPoint scale point =
    let displacement = vectorFrom centerPoint point
    translateBy (Vector2d.scaleBy scale displacement) centerPoint

let along (axis: Axis2d) distance =
    translateBy (Vector2d.in_ axis.Direction distance) axis.OriginPoint

let distanceAlong (axis: Axis2d) =
    vectorFrom axis.OriginPoint >> Vector2d.componentIn axis.Direction

let signedDistanceFrom (axis: Axis2d) point =
    let displacement = vectorFrom axis.OriginPoint point
    let directionVector = Direction2d.toVector axis.Direction
    Vector2d.crossProduct directionVector displacement

let in_ (frame: Frame2d) (x, y) =
    let xVector = Vector2d.in_ frame.XDirection x
    let yVector = Vector2d.in_ frame.YDirection y
    frame.OriginPoint
        |> translateBy xVector
        |> translateBy yVector

//  rotateAround
//  translateBy
//  mirrorAcross
//  projectOnto
//  relativeTo
//  placeIn
//  placeOnto
//  hull
