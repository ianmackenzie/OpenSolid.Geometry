module OpenSolid.Axis2d

open OpenSolid.Vector2d
open OpenSolid.Direction2d
open OpenSolid.Point2d

type Axis2d(originPoint : Point2d, direction : Direction2d) =
    member inline internal this._originPoint =
        originPoint

    member inline internal this._direction =
        direction

    static member originPoint(axis : Axis2d) =
        axis._originPoint

    static member direction(axis : Axis2d) =
        axis._direction

type Point2d with
    static member along axis distance =
        Axis2d.originPoint axis + distance * Axis2d.direction axis

    static member distanceAlong axis point =
        Point2d.vectorFrom (Axis2d.originPoint axis) point
            |> Vector2d.componentIn (Axis2d.direction axis)

    static member signedDistanceFrom axis point =
        let displacement = Point2d.vectorFrom (Axis2d.originPoint axis) point
        let directionVector = Direction2d.toVector (Axis2d.direction axis)
        Vector2d.crossProduct directionVector displacement



