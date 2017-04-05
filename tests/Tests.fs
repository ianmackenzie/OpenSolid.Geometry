module Tests

open System
open Xunit
open OpenSolid
open OpenSolid.Vector2d

[<Fact>]
let ``Equality works as expected`` () =
    let v1a = Vector2d(2.0, 3.0)
    let v1b = Vector2d(2.0, 3.0)
    let v2 = Vector2d(1.0, 2.0)
    let p1a = Point2d(2.0, 3.0)
    let p1b = Point2d(2.0, 3.0)
    let p2 = Point2d(1.0, 2.0)

    Assert.Equal(v1a, v1a)
    Assert.Equal(v1a, v1b)
    Assert.NotEqual(v1a, v2)
    Assert.NotEqual(v1a :> obj, p1a :> obj)

    Assert.Equal(p1a, p1a)
    Assert.Equal(p1a, p1b)
    Assert.NotEqual(p1a, p2)
    Assert.NotEqual(p1a :> obj, v1a :> obj)

    Assert.Equal(Vector2d(3.0, 4.0).Length, 5.0)
    Assert.Equal(Vector2d(2.0, 3.0).Dot(Vector2d(1.0, 1.0)), 5.0)
