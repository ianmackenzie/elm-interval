module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Interval exposing (Interval)
import Interval.Decode as Decode
import Interval.Encode as Encode
import Json.Decode as Decode
import Test exposing (Test)


fuzzer : Fuzzer Interval
fuzzer =
    let
        endpoint =
            Fuzz.floatRange -10 10
    in
    Fuzz.map2 Interval.from endpoint endpoint


endpointsString : Interval -> String
endpointsString interval =
    let
        ( minValue, maxValue ) =
            Interval.endpoints interval
    in
    "[" ++ toString minValue ++ "," ++ toString maxValue ++ "]"


expectValueIn : Interval -> Float -> Expectation
expectValueIn interval value =
    let
        ( minValue, maxValue ) =
            Interval.endpoints interval

        tolerance =
            1.0e-12
    in
    if minValue - tolerance <= value && value <= maxValue + tolerance then
        Expect.pass
    else
        Expect.fail
            (toString value
                ++ " is not contained in the interval "
                ++ endpointsString interval
            )


jsonRoundTrips : Test
jsonRoundTrips =
    Test.fuzz fuzzer
        "JSON conversion round-trips properly"
        (\interval ->
            Encode.interval interval
                |> Decode.decodeValue Decode.interval
                |> Expect.equal (Ok interval)
        )


sinWorksProperly : Test
sinWorksProperly =
    Test.fuzz2
        fuzzer
        (Fuzz.floatRange 0 1)
        "sin works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
            in
            sin valueInInterval |> expectValueIn (Interval.sin interval)
        )


cosWorksProperly : Test
cosWorksProperly =
    Test.fuzz2
        fuzzer
        (Fuzz.floatRange 0 1)
        "cos works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Interval.interpolate interval t
            in
            cos valueInInterval |> expectValueIn (Interval.cos interval)
        )
