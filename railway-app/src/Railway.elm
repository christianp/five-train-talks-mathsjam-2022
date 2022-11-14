module Railway exposing 
    ( RailwayPiece(..)
    , RailwaySegment
    , Orientation
    , RailwayAlignment(..)
    , railway_length
    , railway_piece_length
    , railway_piece_path
    , railway_piece_turn
    , railway_piece_end
    , railway_piece_curvature
    , railway_end
    , align_railway
    , point_on_railway
    , curvature_on_railway
    )

import Parameters exposing (..)
import Tuple exposing (first, second)
import Util exposing (strf, ff, sign)
import Vector exposing (Coords, rotate, vadd)

-- A type alias for an orientation: an angle in radians
type alias Orientation = Float

-- The different kinds of railway pieces
type RailwayPiece
    = Straight Float
    | CurveLeft Float {- 45 degree turn anti-clockwise, with given radius of curvature -}
    | CurveRight Float {- 45 degree turn clockwise, with given radius of curvature -}
    | Clothoid Float Float {- clothoid with given rate of change of curvature and length -}

-- A segment of a railway
type alias RailwaySegment = List RailwayPiece

-- How should a segment of railway be aligned to a point?
type RailwayAlignment
    = AlignStart
    | AlignEnd

clothoid_points : Orientation -> Float -> Float -> Float -> List Coords
clothoid_points so turn length d =
    let
        num_steps = ceiling d
        step n (o,(x,y),points) =
            let
                st = (toFloat 1)/(toFloat num_steps)
                t = (toFloat n)*st*d/length
                s = d*st
                l = d*t
                r = turn*(t*t)
                p = (s*(cos o), s*(sin o))
            in
                (r, p, p::points)
    in
        List.foldl step (so,(0,0),[]) (List.range 0 num_steps) |> \(_,_,points) -> List.reverse points

{-
The difference in orientation between the start and end of a piece
-}
railway_piece_turn : RailwayPiece -> Orientation
railway_piece_turn p = case p of
    CurveLeft _ -> -pi/4
    CurveRight _ -> pi/4
    Straight _ -> 0
    Clothoid turn _ -> turn

{-
The radius of curvature of a piece of railway.
-}
railway_piece_curvature : RailwayPiece -> Float -> Float
railway_piece_curvature p d = case p of
    Straight _ -> 1/0
    CurveLeft r -> r
    CurveRight r -> r
    Clothoid turn length -> -(length*length/turn/(d))

{-
A portion of an SVG path representing a railway piece.

Represented as relative moves, so they can be strung together without knowing any context.
-}
railway_piece_path : (Orientation, RailwayPiece) -> String
railway_piece_path (o,p) = 
    let
        curve_radius = railway_piece_curvature p 0
        (cx,cy) = railway_piece_end o p
    in
        case p of
            Straight _ -> 
                strf "l % %" [ff cx, ff cy]
            CurveLeft _ ->
                strf "a % % 0 0 0 % %" (List.map ff [curve_radius, curve_radius, cx, cy])
            CurveRight _ ->
                strf "a % % 0 0 1 % %" (List.map ff [curve_radius, curve_radius, cx, cy])
            Clothoid turn length ->
                let
                    points = clothoid_points o turn length length
                in
                    String.join " " <|
                        (List.map (\(dx,dy) -> strf "l % %" [ff dx, ff dy]) points)
                    

railway_piece_end : Orientation -> RailwayPiece -> Coords
railway_piece_end o p =
    let
        l = railway_piece_length p
        a = railway_piece_turn p
        curve_radius = railway_piece_curvature p 0
        (tx,ty) = (curve_radius * (sin (abs a)), curve_radius * (sign a) * (1-(cos a)))
        (cx,cy) = rotate o (tx,ty)
    in
        case p of
            Straight _ -> 
                ((cos o)*l, (sin o)*l)
            CurveLeft _ ->
                (cx, cy)
            CurveRight _ ->
                (cx, cy)
            Clothoid turn length -> (cx,cy) -- TODO

railway_end : Orientation -> Coords -> List RailwayPiece -> (Orientation, Coords)
railway_end init_o init_pos pieces =
    let
        handle p (o,pos) =
            ( o+(railway_piece_turn p)
            , vadd pos (railway_piece_end o p)
            )
    in
        List.foldl handle (init_o, init_pos) pieces

{-
The length of a piece of railway, to calculate how far the train has to go.
-}
railway_piece_length : RailwayPiece -> Float
railway_piece_length p = case p of
    Straight l -> l
    CurveLeft curve_radius -> curve_radius * (abs (railway_piece_turn p))
    CurveRight curve_radius -> curve_radius * (abs (railway_piece_turn p))
    Clothoid _ length -> length

-- Add up the lengths of a list of pieces to get the total length.
railway_length : List RailwayPiece -> Float
railway_length = List.sum << (List.map railway_piece_length)

{-
Get the coordinates of a point on a railway piece, given the distance along the track from the start point.

The distance is clamped between 0 and the piece's length, so it'll always return a point on the piece.
This means you can pass in a big negative distance and it'll just stay at the start point.

The pieces are all either straight or a circular segment, so they're easy to interpolate.
-}
point_on_railway_piece : (Orientation, RailwayPiece) -> Float -> Coords
point_on_railway_piece (o,piece) d =
    let
        t = clamp 0 1 (d/(railway_piece_length piece))
        a = (railway_piece_turn piece)*t
        curve_radius = railway_piece_curvature piece d
        (tx,ty) = (curve_radius * (sin (abs a)), curve_radius * (sign a) * (1-(cos a)))
        (cx,cy) = rotate o (tx,ty)
    in
        case piece of
            Straight l -> (t*l*(cos (o+a)), t*l*(sin (o+a)))
            CurveLeft _ -> (cx, cy)
            CurveRight _ -> (cx, cy)
            Clothoid turn length -> 
                let
                    points = clothoid_points o turn length (t*length)
                    x = List.sum (List.map first points)
                    y = List.sum (List.map second points)
                in
                    (x, y)
{-
Given a list of connected railway pieces and a distance along it from the start of the first piece, get the coordinates and orientation of the corresponding point.

Working through each piece, count down ``d`` until it's within the bounds of the piece you're looking at.
Then stick with the coordinates on that piece.
-}
point_on_railway : Orientation -> Coords -> List RailwayPiece -> Float -> (Coords, Orientation)
point_on_railway so (sx, sy) pieces sd =
    let
        handle_piece : RailwayPiece -> (Coords, Orientation, Float) -> (Coords, Orientation, Float)
        handle_piece p ((x, y), o, d) = 
            if d>0 then
                let
                    l = railway_piece_length p
                    (dx, dy) = point_on_railway_piece (o,p) d
                    turn = railway_piece_turn p
                in
                    ((x+dx, y+dy), angle_on_railway_piece o p d, d-l)
            else
                ((x, y), o, d)
    in
        List.foldl handle_piece ((sx, sy), so, sd) pieces |> \(pos, o, _) -> (pos, o)

curvature_on_railway : List RailwayPiece -> Float -> Float
curvature_on_railway pieces sd =
    let
        handle_piece : RailwayPiece -> (Float, Float) -> (Float, Float)
        handle_piece piece (d, c) =
            if d>0 then
                let
                    l = railway_piece_length piece
                in
                    (d-l, railway_piece_curvature piece d)
            else
                (d, c)
    in
        List.foldl handle_piece (sd,0) pieces |> second

{-
Do the same thing, but returning the angle of the tangent to the track at that point.
-}
angle_on_railway_piece : Orientation -> RailwayPiece -> Float -> Float
angle_on_railway_piece o piece d = 
    let
        t = clamp 0 1 (d/(railway_piece_length piece))
        a = railway_piece_turn piece
    in
        case piece of
            Clothoid turn length ->
                o + turn*(t*t)

            _ -> o + t*a

align_railway : (Orientation, Coords) -> RailwayAlignment -> RailwaySegment -> (Orientation, Coords)
align_railway (o1, (x1,y1)) alignment route = case alignment of
    AlignStart ->
        (o1, (x1, y1))
    AlignEnd -> 
        let
            (o2,(x2,y2)) = railway_end 0 (0,0) route
            (dx,dy) = rotate (-o2) (x2,y2)
        in
            (o1-o2, (x1-dx, y1-dy))
