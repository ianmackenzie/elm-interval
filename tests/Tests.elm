module Tests exposing
    ( aggregateN
    , cosWorksProperly
    , endpointsString
    , expectContainedIn
    , expectDistinct
    , expectValueIn
    , fuzzer
    , hullN
    , intersection
    , intersectsAndIntersectionAreConsistent
    , sinWorksProperly
    , union
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Interval exposing (Interval)
import Test exposing (Test)


fuzzer : Fuzzer (Interval Float)
fuzzer =
    let
        endpoint =
            Fuzz.floatRange -10 10
    in
    Fuzz.map2 Interval.from endpoint endpoint


endpointsString : Interval Float -> String
endpointsString interval =
    let
        ( minValue, maxValue ) =
            Interval.endpoints interval
    in
    "[" ++ String.fromFloat minValue ++ "," ++ String.fromFloat maxValue ++ "]"


expectValueIn : Interval Float -> Float -> Expectation
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
            (String.fromFloat value
                ++ " is not contained in the interval "
                ++ endpointsString interval
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


hullN : Test
hullN =
    Test.fuzz (Fuzz.list Fuzz.float)
        "containing"
        (\values ->
            case Interval.hullN values of
                Just interval ->
                    interval
                        |> Expect.all
                            (List.map
                                (\value ->
                                    \theInterval ->
                                        value |> expectValueIn theInterval
                                )
                                values
                            )

                Nothing ->
                    values |> Expect.equal []
        )


union : Test
union =
    Test.describe "union"
        [ Test.fuzz3
            fuzzer
            fuzzer
            (Fuzz.floatRange 0 1)
            "Value in first interval is in union"
            (\firstInterval secondInterval t ->
                Interval.interpolate firstInterval t
                    |> expectValueIn
                        (Interval.union firstInterval secondInterval)
            )
        , Test.fuzz3
            fuzzer
            fuzzer
            (Fuzz.floatRange 0 1)
            "Value in second interval is in union"
            (\firstInterval secondInterval t ->
                Interval.interpolate secondInterval t
                    |> expectValueIn
                        (Interval.union firstInterval secondInterval)
            )
        ]


expectDistinct : Interval Float -> Interval Float -> Expectation
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


expectContainedIn : Interval Float -> Interval Float -> Expectation
expectContainedIn firstInterval secondInterval =
    if Interval.isContainedIn firstInterval secondInterval then
        Expect.pass

    else
        Expect.fail <|
            "Interval "
                ++ endpointsString secondInterval
                ++ " is not contained in "
                ++ endpointsString firstInterval


aggregateN : Test
aggregateN =
    Test.fuzz (Fuzz.list fuzzer)
        "aggregateN"
        (\intervals ->
            case Interval.aggregateN intervals of
                Just aggregateInterval ->
                    aggregateInterval
                        |> Expect.all
                            (List.map
                                (\interval ->
                                    \theAggregateInterval ->
                                        interval
                                            |> expectContainedIn
                                                theAggregateInterval
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

                maybeIntersection =
                    Interval.intersection firstInterval secondInterval
            in
            intersects |> Expect.equal (maybeIntersection /= Nothing)
        )
