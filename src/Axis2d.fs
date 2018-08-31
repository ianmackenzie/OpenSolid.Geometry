[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OpenSolid.Axis2d

let through originPoint direction =
    Axis2d ( originPoint = originPoint, direction = direction )

let withDirection direction originPoint =
    Axis2d ( originPoint = originPoint, direction = direction )

let x =
    through Point2d.origin Direction2d.x

let y =
    through Point2d.origin Direction2d.y

let originPoint (Axis2d ( originPoint = originPoint )) =
    originPoint

let direction (Axis2d ( direction = direction )) =
    direction
