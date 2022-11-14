module Vector exposing (Coords, rotate, vadd)

-- A type alias to conveniently refer to a pair of coordinates
type alias Coords = (Float, Float)

{-
Rotate a vector
-}
rotate : Float -> Coords -> Coords
rotate o (x,y) =
    let
        c = cos o
        s = sin o
    in
        (c*x - s*y, s*x + c*y)

vadd : Coords -> Coords -> Coords
vadd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

