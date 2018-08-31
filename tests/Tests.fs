module Tests

open System
open Xunit
open OpenSolid

[<Fact>]
let ``Equality works as expected`` () =
    let v1a = Vector2d.fromComponents ( 2.0, 3.0 )
    let v1b = Vector2d.fromComponents ( 2.0, 3.0 )
    let v2 = Vector2d.fromComponents ( 1.0, 2.0 )
    let p1a = Point2d.fromCoordinates ( 2.0, 3.0 )
    let p1b = Point2d.fromCoordinates ( 2.0, 3.0 )
    let p2 = Point2d.fromCoordinates ( 1.0, 2.0 )

    Assert.Equal(v1a, v1a)
    Assert.Equal(v1a, v1b)
    Assert.NotEqual(v1a, v2)
    Assert.NotEqual(v1a :> obj, p1a :> obj)

    Assert.Equal(p1a, p1a)
    Assert.Equal(p1a, p1b)
    Assert.NotEqual(p1a, p2)
    Assert.NotEqual(p1a :> obj, v1a :> obj)

[<Fact>]
let ``Length works as expected`` () =
    let vector = Vector2d.fromComponents ( 3.0, 4.0 )
    Assert.Equal(Vector2d.length vector, 5.0)

[<Fact>]
let ``Dot product works as expected`` () =
    let v1 = Vector2d.fromComponents ( 2.0, 3.0 )
    let v2 = Vector2d.fromComponents ( 1.0, 1.0 )
    Assert.Equal(Vector2d.dotProduct v1 v2, 5.0)

[<Fact>]
let ``Projection onto axis works as expected`` () =
    let point = Point2d.fromCoordinates ( 3.0, 4.0 )
    let expected = Point2d.fromCoordinates ( 3.0, 0.0 )
    Assert.Equal(Point2d.projectOnto Axis2d.x point, expected)
