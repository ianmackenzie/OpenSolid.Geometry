[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Point2d

let fromCoordinates coordinates =
    Point2d coordinates

let fromPolarCoordinates ( r, theta ) =
    fromCoordinates ( r * cos theta, r * sin theta )

let coordinates (Point2d coordinates) =
    coordinates

let xCoordinate (Point2d ( x, _ )) =
    x

let yCoordinate (Point2d ( _, y )) =
    y

let origin =
    fromCoordinates ( 0.0, 0.0 )

let interpolateFrom firstPoint secondPoint parameter =
    let ( x1, y1 ) = coordinates firstPoint
    let ( x2, y2 ) = coordinates secondPoint
    let x = Scalar.interpolateFrom x1 x2 parameter
    let y = Scalar.interpolateFrom y1 y2 parameter
    fromCoordinates ( x, y )

let midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5

let squaredDistanceFrom firstPoint secondPoint =
    Vector2d.squaredLength (Vector2d.from firstPoint secondPoint)

let distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)

let equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance

let translateBy vector point =
    let ( vx, vy ) = Vector2d.components vector
    let ( px, py ) = coordinates point
    fromCoordinates ( px + vx, py + vy )

let scaleAbout centerPoint scale point =
    let displacement = Vector2d.from centerPoint point
    translateBy (Vector2d.scaleBy scale displacement) centerPoint

let along axis distance =
    let (Axis2d( originPoint, direction )) = axis
    let displacement = Vector2d.withLength distance direction
    translateBy displacement originPoint

let distanceAlong axis point =
    let (Axis2d ( originPoint, direction )) = axis
    Vector2d.from originPoint point |> Vector2d.componentIn direction

let signedDistanceFrom axis point =
    let (Axis2d ( originPoint, direction )) = axis
    let displacement = Vector2d.from originPoint point
    let directionVector = Direction2d.toVector direction
    Vector2d.crossProduct directionVector displacement

let fromCoordinatesIn frame ( x, y ) =
    let (Frame2d ( originPoint, xDirection, yDirection )) = frame
    let ( x0, y0 ) = coordinates originPoint
    let ( x1, y1 ) = Direction2d.components xDirection
    let ( x2, y2 ) = Direction2d.components yDirection
    fromCoordinates ( x0 + x * x1 + y * x2, y0 + x * y1 + y * y2 )

let projectOnto axis point =
    let (Axis2d ( originPoint, direction )) = axis
    let displacement = Vector2d.from originPoint point
    along axis (Vector2d.componentIn direction displacement)


let axisDirection (Axis2d ( direction = direction )) =
    direction

//  rotateAround
//  translateBy
//  mirrorAcross
//  projectOnto
//  relativeTo
//  placeIn
//  placeOnto
//  hull
