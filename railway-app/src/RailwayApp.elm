port module RailwayApp exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Direction exposing (Direction(..))
import Ease
import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Parameters exposing (..)
import Parser as P exposing (Parser, (|.), (|=))
import Railway exposing
    ( RailwayPiece(..)
    , RailwaySegment
    , Orientation
    , RailwayAlignment(..)
    , railway_length
    , railway_piece_length
    , railway_piece_path
    , railway_piece_turn
    , railway_piece_curvature
    , railway_piece_end
    , railway_end
    , align_railway
    , point_on_railway
    )
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import SvgPath
import Token exposing (Token(..), BinaryOp, tokenise)
import Train exposing
    ( TrackAction(..)
    , TrainAction(..)
    , Acceleration(..)
    , Route
    , Train
    , train_distance
    , train_position
    , action_time
    )
import Tuple exposing (first, second, pair)
import Util exposing (..)
import Vector exposing (Coords, vadd)

port visibilityChange : (Bool -> msg) -> Sub msg

-- The app will take over the whole document
main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Options =
    { show_curvature : Bool
    , viewBox : { left: Float, right: Float, top: Float, bottom: Float }
    , show_controls : Bool
    , rail_style : RailStyle
    }

type alias Model =
    { time : Float
    , trains : List Train
    , train_speed : Float
    , rail_segments: List ((Orientation, Coords), RailwaySegment)
    , options: Options
    , app_visible: Bool
    , rendered_railway : Html Msg
    }

type Msg
    = SetTrainSpeed String
    | Frame Float
    | VisibilityChange Bool

type alias SegmentDefinition = (RailwayAlignment, RailwaySegment)

parse_railway_segments : String -> (List SegmentDefinition)
parse_railway_segments = parse_lines parse_railway_segment

parse_railway_segment : Parser SegmentDefinition
parse_railway_segment =
    P.succeed (\alignment -> \pieces -> (alignment, pieces))
    |= P.oneOf
        [ P.succeed AlignStart
          |. P.keyword "start"
        , P.succeed AlignEnd
          |. P.keyword "end"
        ]
    |. P.spaces
    |= parse_railway

parse_railway : Parser (RailwaySegment)
parse_railway = 
    P.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = P.spaces
        , item = parse_railway_piece
        , trailing = P.Optional
        }

parse_railway_curve keyword constructor =
    P.succeed constructor
      |. P.keyword keyword
      |= (P.oneOf 
            [ P.succeed identity
              |. P.spaces
              |= P.float
            , P.succeed 100
            ]
         )

parse_railway_piece : Parser RailwayPiece
parse_railway_piece =
    P.oneOf
        [ P.succeed Straight
          |. P.keyword "S" 
          |. P.spaces
          |= P.float

        , parse_railway_curve "L" CurveLeft
        , parse_railway_curve "R" CurveRight

        , P.succeed Clothoid
          |. P.keyword "C"
          |. P.spaces
          |= (P.map degrees P.float)
          |. P.spaces
          |= P.float
        ]

parse_list : Parser a -> List String -> Result (List P.DeadEnd) (List a)
parse_list parser inputs =
    let
        step : String -> Result (List P.DeadEnd) (List a) -> Result (List P.DeadEnd) (List a)
        step input res =
            Result.andThen (\bits -> Result.map ((\x -> [x]) >> (++) bits) (P.run parser input)) res
    in
        List.foldl step (Ok []) inputs

type alias UnplacedSegment = (RailwayAlignment, RailwaySegment)
type alias PlacedSegment = ((Orientation, Coords), RailwaySegment)

parse_train : List UnplacedSegment -> Parser Train
parse_train segments =
    let
        make_train : List ((Orientation, Coords), TrainAction) -> Train
        make_train actions =
            let
                (o,pos) = (List.filter (second >> is_follow_route) >> List.head >> Maybe.map first >> Maybe.withDefault (0,(0,0))) actions
            in
                train_with_route pos o (List.map second actions)
    in
        P.succeed make_train
        |= P.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = P.spaces
            , item = parse_train_action segments
            , trailing = P.Optional
            }

parse_train_action : List UnplacedSegment -> Parser ((Orientation, Coords), TrainAction)
parse_train_action segments =
    let
        make_follow acceleration direction ns =
            let
                n = ns |> List.head |> Maybe.withDefault -1
                (alignment, segment) = (List.drop n >> List.head >> Maybe.withDefault (AlignStart, [])) segments
                (o,pos) = align_railway (0, (0,0)) alignment segment
                route = segments |> List.indexedMap pair |> List.filter (first >> (\i -> List.member i ns)) |> List.map (second >> second) |> List.concat
            in
                ((o,pos), FollowRoute acceleration direction o route)
    in
        P.oneOf
            [ P.succeed make_follow
              |. P.keyword "follow"
              |. P.spaces
              |= P.oneOf
                [ P.succeed StopStart |. P.keyword "stopstart"
                , P.succeed Continuous |. P.keyword "continuous"
                ]
              |. P.spaces
              |= P.oneOf
                [ P.succeed Forwards |. P.keyword "forwards"
                , P.succeed Backwards |. P.keyword "backwards"
                ]
              |. P.spaces
              |= P.oneOf
                  [ P.succeed List.singleton
                    |= P.int
                  , P.sequence
                    { start = "["
                    , separator = ","
                    , end = "]"
                    , spaces = P.spaces
                    , item = P.int
                    , trailing = P.Optional
                    }
                  ]

            , P.succeed (\t -> ((0,(0,0)), Wait t))
              |. P.keyword "wait"
              |. P.spaces
              |= P.float
            ]

train_with_route : Coords -> Orientation -> Route -> Train
train_with_route position orientation route =
    { route = route
    , start_position = position
    , orientation = orientation
    }

parse_lines : Parser a -> String -> List a
parse_lines parser = String.split "\n" >> List.map String.trim >> List.filter (String.isEmpty >> not) >> parse_list parser >> Result.withDefault []

init_model : Flags -> Model
init_model flags =
    let
        aligned_segments : List PlacedSegment
        aligned_segments = List.map (\(alignment, pieces) -> (align_railway (0, (0,0)) alignment pieces, pieces)) flags.segments

        options = options_from_flags flags

        rendered_railway = view_tracks options aligned_segments
    in
        { time = 0
        , trains = flags.trains
        , train_speed = flags.speed
        , rail_segments = aligned_segments
        , options = options
        , app_visible = True
        , rendered_railway = rendered_railway
        }

all_routes_time : Model -> Float
all_routes_time = .trains >> List.map (.route >> List.map Train.action_time >> List.sum) >> List.maximum >> Maybe.withDefault 0

init_options =
    { show_curvature = False
    , viewBox = { left = -750, right = 120, top = -300, bottom = 100 }
    }

init_segment_def = 
    """
    end (S 500, R 100, S 200, L 100)
    end (S 1000)
    start (S 500)
    """

init_train_def =
    """
    (follow continuous forwards 0, follow continuous forwards 2)
    (wait 800, follow continuous forwards [1,2])
    """

type RailStyle
    = Sleepers
    | Brio

type alias Flags = 
    { segments : List SegmentDefinition
    , trains : List Train
    , show_curvature : Bool
    , viewBox : { left: Float, right: Float, top: Float, bottom: Float }
    , speed : Float
    , show_controls : Bool
    , rail_style : RailStyle
    }

options_from_flags : Flags -> Options
options_from_flags flags = 
    { show_curvature = flags.show_curvature
    , viewBox = flags.viewBox
    , show_controls = flags.show_controls
    , rail_style = flags.rail_style
    }

blank_viewBox = { left = 0, right = 0, top = 0, bottom = 0 }

blank_flags : Flags
blank_flags = 
    { segments = []
    , trains = [] 
    , show_curvature = False
    , viewBox = blank_viewBox
    , speed = 0
    , show_controls = True
    , rail_style = Sleepers
    }

decode_viewBox =
    JD.string |> JD.map (String.split " " >> List.filterMap String.toFloat) |> JD.andThen (\bits -> case bits of
        left::right::top::bottom::_ -> JD.succeed { left = left, right = right, top = top, bottom = bottom }
        _ -> JD.fail "Bad viewBox"
    )

decode_flags : JD.Decoder Flags
decode_flags = 
       (JD.field "segments" JD.string |> JD.map (parse_railway_segments))
    |> JD.andThen (\segments ->
        JD.map6 (Flags segments) 
            (JD.field "trains" JD.string |> JD.map (parse_lines (parse_train segments)))
            (JD.oneOf
                [ JD.field "show_curvature" JD.bool
                , JD.succeed False
                ]
            )
            (JD.oneOf
                [ JD.field "viewBox" decode_viewBox
                , JD.succeed blank_viewBox
                ]
            )
            (JD.oneOf
                [ JD.field "speed" JD.string |> JD.andThen (String.toFloat >> \m -> case m of
                    Just n -> JD.succeed n
                    Nothing -> JD.fail "bad speed")
                , JD.succeed 0
                ]
            )
            (JD.oneOf
                [ JD.field "show_controls" JD.bool
                , JD.succeed True
                ]
            )
            (JD.oneOf
                [ JD.field "rail_style"
                    (JD.string |> JD.andThen (\s ->
                        case s of
                            "sleepers" -> JD.succeed Sleepers
                            "brio" -> JD.succeed Brio
                            _ -> JD.fail "bad rail style"
                    ))
                , JD.succeed Sleepers
                ]
            )
       )
init : JD.Value -> (Model, Cmd Msg)
init encoded_flags = JD.decodeValue decode_flags encoded_flags |> Result.withDefault blank_flags |> init_model |> nocmd
        

nocmd : Model -> (Model, Cmd Msg)
nocmd model = (model, Cmd.none)

clamp_speed s = if abs s < 0.1 then 0 else s

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    SetTrainSpeed s -> case String.toFloat s of
        Just speed -> { model | train_speed = clamp_speed speed } |> nocmd
        Nothing -> nocmd model

    Frame dt ->
        if model.app_visible then
            { model | time = fmod (model.time + dt*model.train_speed) (all_routes_time model) } |> (\m -> if m.train_speed /= 0 then update_trains m else m) |> nocmd
        else
            nocmd model
    
    VisibilityChange visible -> { model | app_visible = visible } |> nocmd

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ onAnimationFrameDelta Frame
    , visibilityChange VisibilityChange
    ]


brio_piece_path : (Coords, Orientation, RailwayPiece) -> Svg Msg
brio_piece_path ((x,y),o,p) = 
    let
        curve_radius = railway_piece_curvature p 0
        (cx, cy) = railway_piece_end o p
        w = track_gauge
        (dx1, dy1) = (sin o, -(cos o))
        eo = o + (railway_piece_turn p)
        (dx2, dy2) = (sin eo, -(cos eo))
        m = SvgPath.m
        l = SvgPath.l
        a = SvgPath.a
        end_edge = 
            [ l ((-w/2)*dx2) ((-w/2)*dy2)
            , a (w/2) (w/2) 0 True True ((-w)*dx2) ((-w)*dy2)
            , l ((-w/2)*dx2) ((-w/2)*dy2)
            ]
        start_edge =
            [ l ((w/2)*dx1) ((w/2)*dy1)
            , a (w/2) (w/2) 0 True False ((w)*dx1) ((w)*dy1)
            , l ((w/2)*dx1) ((w/2)*dy1)
            ]
        begin = m (x+w*dx1) (y+w*dy1)
        path = case p of
            Straight _ -> 
                SvgPath.join <|
                    [ begin
                    , l cx cy
                    ] 
                    ++ end_edge ++
                    [ l (-cx) (-cy)
                    ]
                    ++ start_edge
            CurveLeft _ ->
                SvgPath.join <|
                    [ begin
                    , a (curve_radius-w) (curve_radius-w) 0 False False (cx+w*(dx2-dx1)) (cy+w*(dy2-dy1))
                    ]
                    ++ end_edge ++
                    [ a (curve_radius+w) (curve_radius+w) 0 False True (-cx+w*(dx2-dx1)) (-cy+w*(dy2-dy1))
                    ]
                    ++ start_edge
            CurveRight _ ->
                SvgPath.join <|
                    [ begin
                    , a (curve_radius+w) (curve_radius+w) 0 False True (cx+w*(dx2-dx1)) (cy+w*(dy2-dy1))
                    ]
                    ++ end_edge ++
                    [ a (curve_radius-w) (curve_radius-w) 0 False False (-cx+w*(dx2-dx1)) (-cy+w*(dy2-dy1))
                    ]
                    ++ start_edge
            Clothoid turn length ->
                ""
    in
        Svg.path
            [ SA.d path
            , SA.stroke "#222"
            , SA.fill "#eee"
            ]
            []

brio_rails : ((Orientation, Coords), RailwaySegment) -> Svg Msg
brio_rails ((io, (x, y)), pieces) =
    let
        handle : RailwayPiece -> (Coords, Orientation, List (Svg Msg)) -> (Coords, Orientation, List (Svg Msg))
        handle p (c, o, paths) = 
            ( vadd c (railway_piece_end o p)
            , o + (railway_piece_turn p)
            , paths++[brio_piece_path (c, o, p)]
            )
    in
        List.foldl handle ((0,0),io,[]) pieces |> (\(c,o,s) -> s) |> Svg.g []

railway_path : Orientation -> Coords -> RailwaySegment -> String
railway_path io (x,y) pieces = 
    let
        handle : RailwayPiece -> (Orientation, List String) -> (Orientation, List String)
        handle p (o, paths) = 
            ( o + (railway_piece_turn p)
            , paths++[railway_piece_path (o,p)]
            )
    in
        String.join "   " (["M "++(ff x)++" "++(ff y)]++(second <| List.foldl handle (io,[]) pieces))

view_rail_under : ((Orientation, Coords), RailwaySegment) -> Svg Msg
view_rail_under ((o, (x,y)), pieces) =
    let
        d = SA.d <| railway_path o (x,y) pieces
    in
        Svg.path
            [ d
            , SA.stroke "#222"
            , SA.fill "none"
            , SA.strokeWidth <| ff <| track_gauge*3
            ]
            []

view_rails : ((Orientation, Coords), RailwaySegment) -> Svg Msg
view_rails ((o, (x, y)), pieces) =
    let
        d = SA.d <| railway_path o (x,y) pieces
    in
        Svg.g
            [ SA.style "mix-blend-mode: screen" ]
            [ Svg.path
                [ d
                , SA.stroke "#ddd"
                , SA.fill "none"
                , SA.strokeWidth <| ff track_gauge
                ]
                []   
            , Svg.path
                [ d
                , SA.stroke "black"
                , SA.fill "none"
                , SA.strokeWidth <| ff <| track_gauge * 0.9
                ]
                []   
            , Svg.path
                [ d
                , SA.stroke "white"
                , SA.fill "none"
                , SA.strokeWidth <| ff <| track_gauge * 1.3
                , SA.strokeDasharray "2 8"
                , SA.strokeDashoffset "0"
                ]
                []
            ]

debug_block : String -> Html Msg
debug_block content =
    H.pre
        []
        [ H.text content ]

debug model =
    div
        []
        [
        ]

view : Model -> Html Msg
view model = 
    div []
        [ div
            [ HA.id "controls" ]
            (if model.options.show_controls then [ speed_input model ] else [])
        , view_railway model
        , debug model
        ]

speed_input model =
    H.label
        []
        [ H.input
            [ HA.type_ "range"
            , HA.min "-1"
            , HA.max "1"
            , HA.step "0.01"
            , HA.value <| ff model.train_speed
            , HE.onInput SetTrainSpeed
            , HA.id "speed"
            ]
            []
        ]

update_train : Model -> Train -> Train
update_train model train =
    let
        time = model.time
    in
        train

update_trains model = { model | trains = List.map (update_train model) model.trains }

is_follow_route piece = case piece of
    FollowRoute _ _ _ _ -> True

    _ -> False

view_train : Options -> Float -> Train -> Svg Msg
view_train options time train =
    let
        ((x, y), angle) = train_position time train
        width = train_length
        height = track_gauge * 1.5
        train_fill = "hsl(199.5, 79.4%, 62%)"
        train_stroke = "hsl(199.5, 79.4%, 42%)"

        body = 
            Svg.path
                [ SA.d <| strf "M % % l % % a % % 0 0 1 % % l % % z"(List.map ff [-width/2, -height/2, width*0.9, 0, height*0.6, height*0.6, 0, height, -width*0.9, 0])
                , SA.fill train_fill
                , SA.stroke train_stroke
                , SA.strokeWidth "2"
                ]
                []
        spout = Svg.g []
            [ Svg.circle
                [ SA.cx <| ff <| width*0.45
                , SA.cy <| ff <| 0
                , SA.r <| ff <| height/4
                , SA.fill "hsl(0,0%,50%)"
                , SA.stroke train_stroke
                , SA.strokeWidth "2"
                ]
                []
            , Svg.circle
                [ SA.cx <| ff <| width*0.45
                , SA.cy <| ff <| 0
                , SA.r <| ff <| height/6
                , SA.fill "black"
                ]
                []
            ]
        knobble =
            Svg.circle
                [ SA.cx <| ff <| width*0.1
                , SA.cy <| ff <| 0
                , SA.r <| ff <| height/5
                , SA.fill train_fill
                , SA.stroke train_stroke
                , SA.strokeWidth "2"
                ]
                []
        rivet i =
            Svg.path
                [ SA.d <| strf "M % % a % % 0 0 1 % %" (List.map ff [width*0.3 - (toFloat i)*10, -height/2, height, height, 0 , height])
                , SA.stroke "hsl(0,0%,30%)"
                , SA.strokeWidth "2"
                , SA.fill "none"
                ]
                []

        cab =
            Svg.rect
                [ SA.x <| ff <| -width*0.5
                , SA.y <| ff <| -height*0.6
                , SA.width <| ff <| width*0.3
                , SA.height <| ff <| height*1.2
                , SA.fill train_stroke
                ]
                []

        route_time = train |> (.route >> List.map Train.action_time >> List.sum)

        label =
            Svg.text_
                [ SA.x <| ff 0
                , SA.y <| ff 50
                , SA.fill "white"
                ]
                [ Svg.text <| ff <| curvature ]

        curvature = Train.current_curvature train time
        rickroll_size = min (abs curvature) 20000
        rickroll_speed = 1/(abs curvature)

        rickroll =
            Svg.g
                [ SA.transform <| strf "translate(0 %) rotate(%) " [ff <| (sign curvature)*(-rickroll_size/2), ff <| time * rickroll_speed] 
                , SA.opacity <| ff <| sqrt <| (min 1 (400/(abs curvature)))
                ]
                [ Svg.circle
                    [ SA.cx <| "0"
                    , SA.cy <| "0"
                    , SA.r <| ff (rickroll_size/2)
                    , SA.fill "#333"
                    ]
                    []
                , Svg.image
                    [ SA.xlinkHref "images/rickroll.png"
                    , SA.x <| ff (-rickroll_size/2)
                    , SA.y <| ff (-rickroll_size/2)
                    , SA.width <| ff rickroll_size
                    , SA.height <| ff rickroll_size
                    ]
                    []
                ]
    in
        Svg.g
            [  ]
            [ Svg.g
                [ SA.transform <| "translate("++(ff x)++", "++(ff y)++") rotate(" ++ (ff <| radians_to_degrees angle) ++ ")"
                ]
                ((if options.show_curvature then [rickroll] else [])++[ body ]++(List.map rivet (List.range 0 4))++[ spout, knobble, cab])
            ]

view_railway : Model -> Html Msg
view_railway model =
    let
        rail_segments = model.rail_segments
        vb = model.options.viewBox

        label_offset = track_gauge * 2.2

        t = model.time
    in
        Svg.svg
            [ SA.viewBox <| String.join " " <| List.map ff [vb.left, vb.top, vb.right - vb.left, vb.bottom - vb.top]
            , SA.id "railway"
            ]
            [ model.rendered_railway
            , Svg.g
                []
                (List.map (view_train model.options t) model.trains)
            ]

view_tracks : Options -> List ((Orientation,Coords), RailwaySegment) -> Svg Msg
view_tracks options rail_segments =
    Svg.g
        []
        (case options.rail_style of
            Sleepers -> 
                [ Svg.g
                    []
                    (List.map view_rail_under rail_segments)
                , Svg.g
                    []
                    (List.map view_rails rail_segments)
                ]
            Brio ->
                (List.map brio_rails rail_segments)
        )
