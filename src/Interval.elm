module Interval exposing
    ( Interval
    , singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection
    , endpoints, minValue, maxValue, midpoint, width
    , interpolate
    , sin, cos
    , contains, intersects, isContainedIn
    )

{-|

@docs Interval


# Constructors

@docs singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Interpolation

@docs interpolate


# Arithmetic

@docs sin, cos


# Queries

@docs contains, intersects, isContainedIn

-}

import Float.Extra as Float


{-| Represents a finite, closed interval with a minimum and maximum value, for
example the interval from 3 to 5. An `Interval Int` represents a range of
integers and an `Interval Float` represents a range of floating-point values.
-}
type Interval number
    = Interval ( number, number )


{-| The unit interval, from 0 to 1.

    Interval.unit
    --> Interval.from 0 1

-}
unit : Interval number
unit =
    from 0 1


{-| Construct a zero-width interval containing a single value.

    Interval.singleton 3
    --> Interval.fromEndpoints ( 3, 3 )

-}
singleton : number -> Interval number
singleton value =
    Interval ( value, value )


{-| Construct an interval from its endpoints (the minimum and maximum values of
the interval).

    rgbRange =
        Interval.fromEndpoints ( 0, 255 )

    alphaRange =
        Interval.fromEndpoints ( 0, 1 )

The two values should be given in order but will be swapped if
necessary to ensure a valid interval is returned:

    Interval.endpoints (Interval.fromEndpoints ( 3, 2 ))
    --> ( 2, 3 )

-}
fromEndpoints : ( number, number ) -> Interval number
fromEndpoints givenEndpoints =
    let
        ( firstValue, secondValue ) =
            givenEndpoints
    in
    if firstValue <= secondValue then
        Interval givenEndpoints

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing the two given values (which can be provided
in either order).

    Interval.endpoints (Interval.from 2 5)
    --> ( 2, 5 )

    Interval.endpoints (Interval.from 5 2)
    --> ( 2, 5 )

-}
from : number -> number -> Interval number
from firstValue secondValue =
    if firstValue <= secondValue then
        Interval ( firstValue, secondValue )

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing all values in the given list. If the list
is empty, returns `Nothing`.

    Interval.containingValues [ 2, 1, 3 ]
    --> Just (Interval.from 1 3)

    Interval.containingValues [ -3 ]
    --> Just (Interval.singleton -3)

    Interval.containingValues []
    --> Nothing

-}
containingValues : List number -> Maybe (Interval number)
containingValues values =
    Maybe.map2 from (List.minimum values) (List.maximum values)


{-| Construct an interval containing both of the given intervals.

    firstInterval =
        Interval.from 1 2

    secondInterval =
        Interval.from 3 6

    Interval.hull firstInterval secondInterval
    --> Interval.from 1 6

-}
hull : Interval number -> Interval number -> Interval number
hull firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    Interval ( min min1 min2, max max1 max2 )


{-| Attempt to construct an interval containing all the values common to both
given intervals. If the intervals do not intersect, returns `Nothing`.

    Interval.intersection
        (Interval.from 1 3)
        (Interval.from 2 5)
    --> Just (Interval.from 2 3)

    Interval.intersection
        (Interval.from 1 3)
        (Interval.from 4 7)
    --> Nothing

If the two intervals just touch, a singleton interval will be returned:

    Interval.intersection
        (Interval.from 1 3)
        (Interval.from 3 5)
    --> Just (Interval.singleton 3)

-}
intersection : Interval number -> Interval number -> Maybe (Interval number)
intersection firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval

        maxOfMins =
            max min1 min2

        minOfMaxes =
            min max1 max2
    in
    if maxOfMins <= minOfMaxes then
        Just (Interval ( maxOfMins, minOfMaxes ))

    else
        Nothing


{-| Construct an interval containing all of the intervals in the given list. If
the list is empty, returns `Nothing`.

    Interval.aggregate
        [ Interval.singleton 2
        , Interval.from 3 4
        ]
    --> Just (Interval.from 2 4)

    Interval.aggregate []
    --> Nothing

-}
aggregate : List (Interval number) -> Maybe (Interval number)
aggregate intervals =
    case intervals of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the endpoints of an interval (its minimum and maximum values) as a
tuple. The first value will always be less than or equal to the second.

    ( minValue, maxValue ) =
        Interval.endpoints someInterval

For any interval,

    Interval.endpoints interval

is equivalent to (but more efficient than)

    ( Interval.minValue interval
    , Interval.maxValue interval
    )

-}
endpoints : Interval number -> ( number, number )
endpoints (Interval intervalEndpoints) =
    intervalEndpoints


{-| Get the minimum value of an interval.

    Interval.minValue (Interval.from 1 3)
    --> 1

-}
minValue : Interval number -> number
minValue interval =
    Tuple.first (endpoints interval)


{-| Get the maximum value of an interval.

    Interval.maxValue (Interval.from 1 3)
    --> 3

-}
maxValue : Interval number -> number
maxValue interval =
    Tuple.second (endpoints interval)


{-| Get the midpoint of an interval.

    Interval.midpoint (Interval.from 1 4)
    --> 2.5

-}
midpoint : Interval Float -> Float
midpoint interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMinValue + 0.5 * (intervalMaxValue - intervalMinValue)


{-| Get the width of an interval.

    Interval.width (Interval.from 1 5)
    --> 4

-}
width : Interval number -> number
width interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMaxValue - intervalMinValue


{-| Interpolate between an interval's endpoints; a value of 0.0 corresponds to
the minimum value of the interval, a value of 0.5 corresponds to its midpoint
and a value of 1.0 corresponds to its maximum value. Values less than 0.0 or
greater than 1.0 can be used to extrapolate.

    Interval.interpolate (Interval.from -1 5) 0
    --> -1

    Interval.interpolate (Interval.from -1 5) 0.75
    --> 3.5

    Interval.interpolate (Interval.from -1 5) -0.5
    --> -4

Note that the interpolation is in fact from the minimum value to the maximum,
_not_ "from the first `Interval.from` argument to the second":

    Interval.interpolate (Interval.from 0 10) 0.2
    --> 2

    Interval.interpolate (Interval.from 10 0) 0.2
    --> 2 -- not 8!

-}
interpolate : Interval Float -> Float -> Float
interpolate interval t =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Float.interpolateFrom intervalMinValue intervalMaxValue t


parameter : Interval Float -> Float -> Float
parameter interval value =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    if intervalMinValue < intervalMaxValue then
        (value - intervalMinValue) / (intervalMaxValue - intervalMinValue)

    else if value < intervalMinValue then
        -1 / 0

    else if value > intervalMaxValue then
        1 / 0

    else
        -- value, intervalMinValue and intervalMaxValue are all equal
        0


{-| Check if an interval contains a given value.

    Interval.contains 0 (Interval.from -1 3)
    --> True

    Interval.contains 5 (Interval.from -1 3)
    --> False

The minimum and maximum values of an interval are considered to be contained in
the interval:

    Interval.contains 3 (Interval.from -1 3)
    --> True

-}
contains : number -> Interval number -> Bool
contains value interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMinValue <= value && value <= intervalMaxValue


{-| Check if two intervals touch or overlap (have any values in common).

    Interval.from -5 5
        |> Interval.intersects (Interval.from 0 10)
    --> True

    Interval.from -5 5
        |> Interval.intersects (Interval.from 10 20)
    --> False

Intervals that just touch each other are considered to intersect (this is
consistent with `intersection` which will return a zero-width interval for the
intersection of two just-touching intervals):

    Interval.from -5 5
        |> Interval.intersects (Interval.from 5 10)
    --> True

-}
intersects : Interval number -> Interval number -> Bool
intersects firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    min1 <= max2 && max1 >= min2


{-| Check if the second interval is fully contained in the first.

    Interval.from -5 5
        |> Interval.isContainedIn (Interval.from 0 10)
    --> False

    Interval.from -5 5
        |> Interval.isContainedIn (Interval.from -10 10)
    --> True

Be careful with the argument order! If not using the `|>` operator, the second
example would be written as:

    Interval.isContainedIn (Interval.from -10 10)
        (Interval.from -5 5)
    --> True

-}
isContainedIn : Interval number -> Interval number -> Bool
isContainedIn firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    min1 <= min2 && max2 <= max1


{-| Check if the interval is a singleton (the minimum and maximum values are the
same).

    Interval.isSingleton (Interval.fromEndpoints ( 2, 2 ))
    --> True

    Interval.isSingleton (Interval.fromEndpoints ( 2, 3 ))
    --> False

-}
isSingleton : Interval number -> Bool
isSingleton interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMinValue == intervalMaxValue


{-| Add the given amount to both endpoints of the given interval.

    Interval.shiftBy 3 (Interval.from -1 5)
    --> Interval.from 2 8

-}
shiftBy : number -> Interval number -> Interval number
shiftBy delta interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Interval ( intervalMinValue + delta, intervalMaxValue + delta )


{-| Get the image of sin(x) applied on the interval.

    Interval.sin (Interval.from 0 (degrees 45))
    --> Interval.from 0 0.7071

    Interval.sin (Interval.from 0 pi)
    --> Interval.from 0 1

-}
sin : Interval Float -> Interval Float
sin interval =
    if isSingleton interval then
        singleton (Basics.sin (minValue interval))

    else
        let
            ( includesMin, includesMax ) =
                sinIncludesMinMax interval

            ( intervalMinValue, intervalMaxValue ) =
                endpoints interval

            newMin =
                if includesMin then
                    -1

                else
                    min (Basics.sin intervalMinValue)
                        (Basics.sin intervalMaxValue)

            newMax =
                if includesMax then
                    1

                else
                    max (Basics.sin intervalMinValue)
                        (Basics.sin intervalMaxValue)
        in
        fromEndpoints ( newMin, newMax )


{-| Get the image of cos(x) applied on the interval.

    Interval.cos (Interval.from 0 (degrees 45))
    --> Interval.from 0.7071 1

    Interval.cos (Interval.from 0 pi)
    --> Interval.from -1 1

-}
cos : Interval Float -> Interval Float
cos interval =
    if isSingleton interval then
        singleton (Basics.cos (minValue interval))

    else
        let
            ( includesMin, includesMax ) =
                cosIncludesMinMax interval

            ( intervalMinValue, intervalMaxValue ) =
                endpoints interval

            newMin =
                if includesMin then
                    -1

                else
                    min (Basics.cos intervalMinValue)
                        (Basics.cos intervalMaxValue)

            newMax =
                if includesMax then
                    1

                else
                    max (Basics.cos intervalMinValue)
                        (Basics.cos intervalMaxValue)
        in
        fromEndpoints ( newMin, newMax )


{-| cos(x - pi/2) = sin(x), therefore if cos(interval - pi/2) includes
the maximum/minimum, that means sin(interval) includes the maximum/minimum
accordingly.
-}
sinIncludesMinMax : Interval Float -> ( Bool, Bool )
sinIncludesMinMax interval =
    interval |> shiftBy (-pi / 2) |> cosIncludesMinMax


{-| cos(x + pi) = -cos(x), therefore if cos(interval + pi) includes the maximum,
that means cos(interval) includes the minimum.
-}
cosIncludesMinMax : Interval Float -> ( Bool, Bool )
cosIncludesMinMax interval =
    ( interval |> shiftBy pi |> cosIncludesMax
    , interval |> cosIncludesMax
    )


{-| The maximum of cos(x) is x = 2 pi \* k for every integer k.
If `minValue` and `maxValue` are in different branches
(meaning diffrent values of k), then the interval must pass through
2 pi \* k, which means the interval must include the maximum value.
-}
cosIncludesMax : Interval Float -> Bool
cosIncludesMax interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval

        minBranch =
            floor <| intervalMinValue / (2 * pi)

        maxBranch =
            floor <| intervalMaxValue / (2 * pi)
    in
    minBranch /= maxBranch
