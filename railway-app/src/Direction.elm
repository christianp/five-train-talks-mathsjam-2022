module Direction exposing
    ( Direction(..)
    , reverse
    )

-- Which way is the train driving?
type Direction
    = Forwards
    | Backwards

reverse : Direction -> Direction
reverse d = case d of
    Forwards -> Backwards
    Backwards -> Forwards
