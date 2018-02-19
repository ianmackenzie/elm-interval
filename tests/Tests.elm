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


containingValues : Test
containingValues =
    Test.fuzz (Fuzz.list Fuzz.float)
        "containing"
        (\values ->
            case Interval.containingValues values of
                Just interval ->
                    interval
                        |> Expect.all
                            (List.map
                                (\value ->
                                    \interval -> expectValueIn interval value
                                )
                                values
                            )

                Nothing ->
                    values |> Expect.equal []
        )


hull : Test
hull =
    Test.describe "hull"
        [ Test.fuzz3
            fuzzer
            fuzzer
            (Fuzz.floatRange 0 1)
            "Value in first interval is in hull"
            (\firstInterval secondInterval t ->
                Interval.interpolate firstInterval t
                    |> expectValueIn
                        (Interval.hull firstInterval secondInterval)
            )
        , Test.fuzz3
            fuzzer
            fuzzer
            (Fuzz.floatRange 0 1)
            "Value in second interval is in hull"
            (\firstInterval secondInterval t ->
                Interval.interpolate secondInterval t
                    |> expectValueIn
                        (Interval.hull firstInterval secondInterval)
            )
        ]


expectDistinct : Interval -> Interval -> Expectation
expectDistinct firstInterval secondInterval =
    if
        (Interval.minValue firstInterval > Interval.maxValue secondInterval)
            || (Interval.maxValue firstInterval < Interval.minValue secondInterval)
    then
        Expect.pass
    else
        Expect.fail <|
            "Intervals "
                ++ endpointsString firstInterval
                ++ " and "
                ++ endpointsString secondInterval
                ++ " are not distinct"


intersection : Test
intersection =
    Test.fuzz3
        fuzzer
        fuzzer
        (Fuzz.floatRange 0 1)
        "Value in intersection is in both intervals"
        (\firstInterval secondInterval t ->
            case Interval.intersection firstInterval secondInterval of
                Just intersectionInterval ->
                    Interval.interpolate intersectionInterval t
                        |> Expect.all
                            [ expectValueIn firstInterval
                            , expectValueIn secondInterval
                            ]

                Nothing ->
                    expectDistinct firstInterval secondInterval
        )


expectContainedIn : Interval -> Interval -> Expectation
expectContainedIn firstInterval secondInterval =
    if Interval.isContainedIn firstInterval secondInterval then
        Expect.pass
    else
        Expect.fail <|
            "Interval "
                ++ endpointsString secondInterval
                ++ " is not contained in "
                ++ endpointsString firstInterval


aggregate : Test
aggregate =
    Test.fuzz (Fuzz.list fuzzer)
        "aggregate"
        (\intervals ->
            case Interval.aggregate intervals of
                Just aggregateInterval ->
                    aggregateInterval
                        |> Expect.all
                            (List.map
                                (\interval ->
                                    \aggregateInterval ->
                                        interval
                                            |> expectContainedIn
                                                aggregateInterval
                                )
                                intervals
                            )

                Nothing ->
                    intervals |> Expect.equal []
        )


intersectsAndIntersectionAreConsistent : Test
intersectsAndIntersectionAreConsistent =
    Test.fuzz2
        fuzzer
        fuzzer
        "intersects and intersection are consistent"
        (\firstInterval secondInterval ->
            let
                intersects =
                    Interval.intersects firstInterval secondInterval

                intersection =
                    Interval.intersection firstInterval secondInterval
            in
            intersects |> Expect.equal (intersection /= Nothing)
        )
