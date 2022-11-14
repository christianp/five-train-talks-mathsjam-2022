module Train exposing
    ( TrainAction(..)
    , Route
    , Train
    , TrackAction(..)
    , Acceleration(..)
    , train_distance
    , train_position
    , current_action
    , action_time
    , current_curvature
    )

import Direction exposing (Direction(..))
import Parameters exposing (..)
import Railway exposing
    ( RailwayPiece(..)
    , RailwaySegment
    , Orientation
    , railway_length
    , railway_piece_length
    , railway_piece_path
    , railway_piece_turn
    , railway_piece_end
    , railway_end
    , point_on_railway
    , curvature_on_railway
    )
import Tuple exposing (first)
import Util exposing (strf, ff, sign)
import Vector exposing (Coords)

{-
It'd be nice to smoothly move the carriages on each track, either forwards when a carriage is pulled away, or backwards when a carriage is dropped off.

When the train picks up a carriage, it'll set the track it's at to Advancing, and the track it's going to to Reversing.

The carriages on those tracks will be drawn in the right position based on the distance of the train from the front of the queue.
-}
type TrackAction
    = Stationary
    | Reversing
    | Advancing

type Acceleration
    = StopStart
    | Continuous

{-
Actions for the train: each shunt is translated to a list of these actions.

``FollowRoute`` actions alternate between interpreted as moving forwards and backwards.
-}
type TrainAction
    = FollowRoute Acceleration Direction Orientation RailwaySegment
    | Wait Float

type alias Route = List TrainAction

type alias Train =
    { route : Route
    , start_position: Coords
    , orientation: Orientation
    }

current_action : Train -> Float -> Maybe (TrainAction, Float)
current_action train time =
    let
        handle_action : TrainAction -> (Maybe (TrainAction, Float), Float) -> (Maybe (TrainAction, Float), Float)
        handle_action action (out, t) = if t <= 0 then (out, t) else 
            let
                total_time = action_time action
            in
                if t < total_time then
                    (Just (action,t), t-total_time)
                else
                    (Nothing, t-total_time)
    in
        List.foldl handle_action (Nothing, time) train.route |> first
        
last_follow_action : Train -> Float -> Maybe (TrainAction, Float)
last_follow_action train time =
    let
        handle_action : TrainAction -> (Maybe (TrainAction, Float), Float) -> (Maybe (TrainAction, Float), Float)
        handle_action action (out, t) = if t <= 0 then (out, t) else 
            let
                total_time = action_time action
            in
                case action of
                    FollowRoute _ _ _ _ -> (Just (action,t), t-total_time)
                    _ -> (out, t-total_time)
    in
        List.foldl handle_action (Nothing, time) train.route |> first

{-
How far along its current segment of track should the train be at the given time?

The train starts from standing, accelerates for a bit, then moves at max speed until it decelerates at the same rate to stop at the end of the segment.

The acceleration is calculated so that the train gets to full speed in the length of a single ``ShortHorizontal`` piece.
The equations of motion are used to work out the acceleration that satisfies this, and the time taken::

    distance = (initial velocity = 0)*(time) + 1/2 * acceleration * (time^2)

    final velocity = (initial velocity = 0) + acceleration * time

    so

    acceleration = final velocity / time

    distance = 1/2 * (final velocity / time) * (time^2)
             = 1/2 * (final velocity) * time

    so

    time = 2 * distance * (final velocity)

If the train is moving backwards, it start at 0 and advances to the full distance.

If the train is moving backwards, it starts at full distance and moves backwards to 0.
-}

action_time : TrainAction -> Float
action_time action = case action of
    FollowRoute acceleration direction _ pieces ->
        let
            total_distance = railway_length pieces
        in
            case acceleration of
                StopStart -> 2*accelerate_time + (total_distance-2*accelerate_distance)/max_speed
                Continuous -> total_distance / max_speed

    Wait delay -> delay

train_distance : Float -> TrainAction -> Float
train_distance time action =
    case action of
        FollowRoute acceleration direction _ pieces ->
            let
                total_distance = railway_length pieces
                dt = max 0 time
                total_time = action_time action
                d = 
                    case acceleration of
                        StopStart -> 
                            if dt<accelerate_time then
                                0.5 * Parameters.acceleration * (dt ^ 2)
                            else if dt < total_time - accelerate_time then
                                accelerate_distance + (dt - accelerate_time)*max_speed
                            else if dt >= total_time then
                                total_distance
                            else
                                let
                                    t = (dt - (total_time - accelerate_time))
                                in
                                    total_distance - accelerate_distance + max_speed * t - 0.5 * Parameters.acceleration * (t ^ 2)

                        Continuous -> clamp 0 total_distance (max_speed * dt)
            in
                case direction of
                    Forwards -> d
                    Backwards -> total_distance - d

        _ -> 0

{-
The current coordinates and orientation of the train.

The only wrinkle is to account for whether the train is moving forwards or backwards.
-}
train_position : Float -> Train -> (Coords, Orientation)
train_position time train =
    let
        handle_action : TrainAction -> (Coords, Orientation, Float) -> (Coords, Orientation, Float)
        handle_action action (position, orientation, t) = if t <= 0 then (position,orientation,t) else 
            let
                total_time = action_time action

                d = train_distance t action

                (npos, no) = case action of
                    FollowRoute _ direction io pieces ->
                        let
                            ((ox, oy), _) = point_on_railway io (0, 0) pieces (railway_length pieces)
                            ((x, y), o) = point_on_railway io position pieces d
                            (dx, dy) = case direction of
                                Forwards -> (0, 0)
                                Backwards -> (-ox, -oy)
                        in
                            ((x+dx, y+dy), o)
                            
                    _ -> (position, orientation)

            in
                (npos, no, t - total_time)

    in
        List.foldl handle_action (train.start_position, train.orientation, time) train.route |> \(p,o,_) -> (p,o)

current_curvature : Train -> Float -> Float
current_curvature train time =
    case last_follow_action train time of
        Just (FollowRoute _ direction _ pieces as action, t) ->
            let
                d = train_distance t action
            in
                curvature_on_railway pieces d

        _ -> 0
