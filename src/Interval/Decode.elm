module Interval.Decode exposing (interval)

{-|

@docs interval

-}

import Interval exposing (Interval)
import Json.Decode as Decode exposing (Decoder)


{-| Decodes an `Interval` from an array of two floating-point endpoints.
-}
interval : Decoder Interval
interval =
    Decode.map2 Interval.from
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
