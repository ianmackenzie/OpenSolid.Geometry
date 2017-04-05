namespace OpenSolid

type Vector2d(xComponent: float, yComponent: float) =
    member this.XComponent = xComponent

    member this.YComponent = yComponent

    static member (~-) (vector: Vector2d) =
        Vector2d(-vector.XComponent, -vector.YComponent)

    static member (+) (firstVector: Vector2d, secondVector: Vector2d) =
        let x1 = firstVector.XComponent
        let y1 = firstVector.YComponent
        let x2 = secondVector.XComponent
        let y2 = secondVector.YComponent
        Vector2d (x1 + x2, y1 + y2)

    static member (-) (firstVector: Vector2d, secondVector: Vector2d) =
        let x1 = firstVector.XComponent
        let y1 = firstVector.YComponent
        let x2 = secondVector.XComponent
        let y2 = secondVector.YComponent
        Vector2d (x1 - x2, y1 - y2)

    static member (*) (scale: float, vector: Vector2d) =
        let x = vector.XComponent
        let y = vector.YComponent
        Vector2d (scale * x, scale * y)

    static member (*) (vector: Vector2d, scale) =
        let x = vector.XComponent
        let y = vector.YComponent
        Vector2d (x * scale, y * scale)

    static member (/) (vector: Vector2d, scale) =
        let x = vector.XComponent
        let y = vector.YComponent
        Vector2d (x / scale, y / scale)

type Direction2d(xComponent: float, yComponent: float) =
    member this.XComponent = xComponent

    member this.YComponent = yComponent

    member this.ComponentIn (other: Direction2d) =
        let x1 = this.XComponent
        let y1 = this.YComponent
        let x2 = other.XComponent
        let y2 = other.YComponent
        x1 * x2 + y1 * y2

    static member (~-) (direction: Direction2d) =
        Direction2d(-direction.XComponent, -direction.YComponent)

    static member (*) (scale: float, direction: Direction2d) =
        let x = direction.XComponent
        let y = direction.YComponent
        Vector2d (scale * x, scale * y)

    static member (*) (direction: Direction2d, scale) =
        let x = direction.XComponent
        let y = direction.YComponent
        Vector2d (x * scale, y * scale)

type Point2d(xCoordinate: float, yCoordinate: float) =
    member this.XCoordinate = xCoordinate

    member this.YCoordinate = yCoordinate

    static member (+) (point: Point2d, vector: Vector2d) =
        let px = point.XCoordinate
        let py = point.YCoordinate
        let vx = vector.XComponent
        let vy = vector.YComponent
        Point2d (px + vx, py + vy)

    static member (-) (point: Point2d, vector: Vector2d) =
        let px = point.XCoordinate
        let py = point.YCoordinate
        let vx = vector.XComponent
        let vy = vector.YComponent
        Point2d (px - vx, py - vy)

    static member (-) (firstPoint: Point2d, secondPoint: Point2d) =
        let x1 = firstPoint.XCoordinate
        let y1 = firstPoint.YCoordinate
        let x2 = secondPoint.XCoordinate
        let y2 = secondPoint.YCoordinate
        Vector2d (x1 - x2, y1 - y2)

type Axis2d(originPoint : Point2d, direction : Direction2d) =
    member this.OriginPoint = originPoint

    member this.Direction = direction

type Frame2d (originPoint: Point2d, xDirection: Direction2d, yDirection: Direction2d) =
    member this.OriginPoint = originPoint

    member this.XDirection = xDirection

    member this.YDirection = yDirection
