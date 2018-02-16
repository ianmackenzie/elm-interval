module Interval.Encode exposing (interval)

{-|

@docs interval

-}

import Interval exposing (Interval)
import Json.Encode as Encode exposing (Value)


{-| Encode an `Interval` as an array of two floating-point endpoints.
-}
interval : Interval -> Value
interval interval_ =
    Encode.list
        [ Encode.float (Interval.minValue interval_)
        , Encode.float (Interval.maxValue interval_)
        ]
