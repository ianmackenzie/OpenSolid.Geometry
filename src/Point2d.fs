[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Point2d

let coordinates (Point2d coordinates) =
    coordinates

let xCoordinate (Point2d (x, _)) =
    x

let yCoordinate (Point2d (_, y)) =
    y

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

let translateBy vector point =
    let (vx, vy) = Vector2d.components vector
    let (px, py) = coordinates point
    Point2d (px + vx, py + vy)

let scaleAbout centerPoint scale point =
    let displacement = vectorFrom centerPoint point
    translateBy (Vector2d.scaleBy scale displacement) centerPoint

let along (Axis2d(originPoint, direction)) distance =
    translateBy (Vector2d.in_ direction distance) originPoint

let distanceAlong (Axis2d(originPoint, direction)) point =
    vectorFrom originPoint point |> Vector2d.componentIn direction

let signedDistanceFrom (Axis2d(originPoint, direction)) point =
    let displacement = vectorFrom originPoint point
    let directionVector = Direction2d.toVector direction
    Vector2d.crossProduct directionVector displacement

let in_ (Frame2d(originPoint, xDirection, yDirection)) (x, y) =
    let (x0, y0) = coordinates originPoint
    let (x1, y1) = Direction2d.components xDirection
    let (x2, y2) = Direction2d.components yDirection
    Point2d (x0 + x * x1 + y * x2, y0 + x * y1 + y * y2)

//  rotateAround
//  translateBy
//  mirrorAcross
//  projectOnto
//  relativeTo
//  placeIn
//  placeOnto
//  hull
