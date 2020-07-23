module Interval exposing
    ( Interval
    , from, fromEndpoints, singleton
    , union, intersection
    , hull, hullN, hullOf, hullOfN, hull3
    , aggregate, aggregateN, aggregateOf, aggregateOfN, aggregate3
    , endpoints, minValue, maxValue, midpoint, width
    , contains, isContainedIn, intersects, isSingleton
    , interpolate, interpolationParameter
    , negate, add, subtract, multiplyBy, divideBy, half, twice
    , plus, minus, sin, cos
    )

{-|

@docs Interval


# Constructors

@docs from, fromEndpoints, singleton


## Booleans

@docs union, intersection


## Hull

These functions let you construct an `Interval` containing one or more input
numbers.

@docs hull, hullN, hullOf, hullOfN, hull3


## Aggregation

These functions let you 'aggregate' one or more intervals into a single larger
interval that contains all of them.

@docs aggregate, aggregateN, aggregateOf, aggregateOfN, aggregate3


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Queries

@docs contains, isContainedIn, intersects, isSingleton


# Interpolation

@docs interpolate, interpolationParameter


# Arithmetic

These functions let you do math with `Interval` values, following the rules of
[interval arithmetic](https://en.wikipedia.org/wiki/Interval_arithmetic).

@docs negate, add, subtract, multiplyBy, divideBy, half, twice
@docs plus, minus, sin, cos

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


{-| Find the interval containing one or more input values:

    Interval.hull 5 [ 3, 2, 4 ]
    --> Interval.from 2 5

Often ends up being used within a `case` expression:

    case values of
        [] ->
            -- some default behavior

        first :: rest ->
            let
                interval =
                    Interval.hull first rest
            in
            -- normal behavior using 'interval'

See also [`hullN`](#hullN).

-}
hull : number -> List number -> Interval number
hull first rest =
    hullHelp first first rest


hullHelp : number -> number -> List number -> Interval number
hullHelp a b values =
    case values of
        value :: rest ->
            hullHelp (min a value) (max b value) rest

        [] ->
            Interval ( a, b )


{-| Construct an interval containing the three given values;

    Interval.hull3 a b c

is equivalent to

    Interval.hull a [ b, c ]

but is more efficient. (If you're looking for a `hull2` function, [`from`](#from)
should do what you want.)

-}
hull3 : number -> number -> number -> Interval number
hull3 a b c =
    Interval ( min a (min b c), max a (max b c) )


{-| Attempt to construct an interval containing all _N_ values in the given
list. If the list is empty, returns `Nothing`. If you know you have at least one
value, you can use [`hull`](#hull) instead.

    Interval.hullN [ 2, 1, 3 ]
    --> Just (Interval.from 1 3)

    Interval.hullN [ -3 ]
    --> Just (Interval.singleton -3)

    Interval.hullN []
    --> Nothing

-}
hullN : List number -> Maybe (Interval number)
hullN values =
    case values of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
number can be extracted from it. For example, if you had

    type alias Person =
        { name : String
        , age : Float
        }

then given some people you could find their range of ages as an `Interval`
using

    Interval.hullOf .age
        firstPerson
        [ secondPerson
        , thirdPerson
        , fourthPerson
        ]

See also [`hullOfN`](#hullOfN).

-}
hullOf : (a -> number) -> a -> List a -> Interval number
hullOf getValue first rest =
    let
        firstValue =
            getValue first
    in
    hullOfHelp firstValue firstValue getValue rest


hullOfHelp : number -> number -> (a -> number) -> List a -> Interval number
hullOfHelp a b getValue list =
    case list of
        first :: rest ->
            let
                value =
                    getValue first
            in
            hullOfHelp (min a value) (max b value) getValue rest

        [] ->
            Interval ( a, b )


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> number) -> List a -> Maybe (Interval number)
hullOfN getValue items =
    case items of
        first :: rest ->
            Just (hullOf getValue first rest)

        [] ->
            Nothing


{-| Construct an interval containing both of the given intervals.

    firstInterval =
        Interval.from 1 2

    secondInterval =
        Interval.from 3 6

    Interval.union firstInterval secondInterval
    --> Interval.from 1 6

(Note that this is not strictly speaking a 'union' in the precise mathematical
sense, since the result will contain values that are _in between_ the two given
intervals and not actually _in_ either of them if those two intervals do not
overlap.)

-}
union : Interval number -> Interval number -> Interval number
union (Interval ( a1, b1 )) (Interval ( a2, b2 )) =
    Interval ( min a1 a2, max b1 b2 )


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
intersection (Interval ( a1, b1 )) (Interval ( a2, b2 )) =
    let
        maxA =
            max a1 a2

        minB =
            min b1 b2
    in
    if maxA <= minB then
        Just (Interval ( maxA, minB ))

    else
        Nothing


{-| Construct an interval containing one or more given intervals:

    Interval.aggregate
        (Interval.singleton 2)
        [ Interval.singleton 4
        , Interval.singleton 3
        ]
    --> Interval.from 2 4

Works much like [`hull`](#hull). See also [`aggregateN`](#aggregateN).

-}
aggregate : Interval number -> List (Interval number) -> Interval number
aggregate (Interval ( a, b )) rest =
    aggregateHelp a b rest


aggregateHelp : number -> number -> List (Interval number) -> Interval number
aggregateHelp a b intervals =
    case intervals of
        (Interval ( c, d )) :: rest ->
            aggregateHelp (min a c) (max b d) rest

        [] ->
            Interval ( a, b )


{-| Special case of [`aggregate`](#aggregate) for the case of three intervals;

    Interval.aggregate3 first second third

is equivalent to

    Interval.aggregate first [ second, third ]

but is more efficient. (If you're looking for an `aggregate2` function,
[`union`](#union) should do what you want.)

-}
aggregate3 :
    Interval number
    -> Interval number
    -> Interval number
    -> Interval number
aggregate3 (Interval ( a1, b1 )) (Interval ( a2, b2 )) (Interval ( a3, b3 )) =
    Interval ( min a1 (min a2 a3), max b1 (max b2 b3) )


{-| Attemp to construct an interval containing all of the intervals in the given
list. If the list is empty, returns `Nothing`. If you know you have at least one
interval, you can use [`aggregate`](#aggregate) instead.
-}
aggregateN : List (Interval number) -> Maybe (Interval number)
aggregateN intervals =
    case intervals of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as an interval can be generated from it (similar to [`hullOf`](#hullOf)).
-}
aggregateOf : (a -> Interval number) -> a -> List a -> Interval number
aggregateOf getInterval first rest =
    let
        (Interval ( a, b )) =
            getInterval first
    in
    aggregateOfHelp a b getInterval rest


aggregateOfHelp : number -> number -> (a -> Interval number) -> List a -> Interval number
aggregateOfHelp a b getInterval items =
    case items of
        first :: rest ->
            let
                (Interval ( c, d )) =
                    getInterval first
            in
            aggregateOfHelp (min a c) (max b d) getInterval rest

        [] ->
            Interval ( a, b )


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> Interval number) -> List a -> Maybe (Interval number)
aggregateOfN getInterval items =
    case items of
        first :: rest ->
            Just (aggregateOf getInterval first rest)

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
minValue (Interval ( a, _ )) =
    a


{-| Get the maximum value of an interval.

    Interval.maxValue (Interval.from 1 3)
    --> 3

-}
maxValue : Interval number -> number
maxValue (Interval ( _, b )) =
    b


{-| Get the midpoint of an interval.

    Interval.midpoint (Interval.from 1 4)
    --> 2.5

-}
midpoint : Interval Float -> Float
midpoint (Interval ( a, b )) =
    a + 0.5 * (b - a)


{-| Get the width of an interval.

    Interval.width (Interval.from 1 5)
    --> 4

-}
width : Interval number -> number
width (Interval ( a, b )) =
    b - a


{-| Interpolate between an interval's endpoints based on a parameter value that
will generally be between 0.0 and 1.0. A value of 0.0 corresponds to the minimum
value of the interval, a value of 0.5 corresponds to its midpoint and a value of
1.0 corresponds to its maximum value:

    Interval.interpolate (Interval.from 1 5) 0
    --> 1

    Interval.interpolate (Interval.from 1 5) 0.75
    --> 4

Values less than 0.0 or greater than 1.0 can be used to extrapolate:

    Interval.interpolate (Interval.from 1 5) 1.5
    --> 7

Note that because of how [`Interval.from`](#from) works, the interpolation is in
fact from the minimum value to the maximum, _not_ "from the first
`Interval.from` argument to the second":

    Interval.interpolate (Interval.from 0 10) 0.2
    --> 2

    Interval.interpolate (Interval.from 10 0) 0.2
    --> 2 -- not 8!

If you want the interpolate from one number down to another, you can use
[`Float.Extra.interpolateFrom`](https://package.elm-lang.org/packages/ianmackenzie/elm-float-extra/latest/Float-Extra#interpolateFrom)
from the `elm-float-extra` package.

-}
interpolate : Interval Float -> Float -> Float
interpolate (Interval ( a, b )) t =
    Float.interpolateFrom a b t


{-| Given an interval and a given value, determine the corresponding
interpolation parameter (the parameter that you would pass to [`interpolate`](#interpolate)
to get the given value):

    Interval.interpolationParameter
        (Interval.from 10 15)
        12
    --> 0.4

The result will be between 0 and 1 if (and only if) the given value is within
the given interval:

    Interval.interpolationParameter
        (Interval.from 10 15)
        18
    --> 1.6

    Interval.interpolationParameter
        (Interval.from 10 15)
        9
    --> -0.2

This is the inverse of `interpolate`; for any non-zero-width `interval`,

    Interval.interpolationParameter interval value
        |> Interval.interpolate interval

should be equal to the original `value` (within numerical roundoff).

-}
interpolationParameter : Interval Float -> Float -> Float
interpolationParameter (Interval ( a, b )) value =
    if a < b then
        (value - a) / (b - a)

    else if value < a then
        -1 / 0

    else if value > b then
        1 / 0

    else
        -- value, a and intervalMaxValue are all equal
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
contains value (Interval ( a, b )) =
    a <= value && value <= b


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
intersects (Interval ( a1, b1 )) (Interval ( a2, b2 )) =
    a1 <= b2 && b1 >= a2


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
isContainedIn (Interval ( a1, b1 )) (Interval ( a2, b2 )) =
    a1 <= a2 && b2 <= b1


{-| Check if the interval is a singleton (the minimum and maximum values are the
same).

    Interval.isSingleton (Interval.from 2 2)
    --> True

    Interval.isSingleton (Interval.from 2 3)
    --> False

-}
isSingleton : Interval number -> Bool
isSingleton (Interval ( a, b )) =
    a == b


{-| Negate an interval. Note that this will flip the order of the endpoints.

    Interval.negate (Interval.from 2 3)
    --> Interval.from -3 -2

-}
negate : Interval number -> Interval number
negate (Interval ( a, b )) =
    Interval ( -b, -a )


{-| Add the given amount to an interval.

    Interval.from -1 5 |> Interval.add 3
    --> Interval.from 2 8

-}
add : number -> Interval number -> Interval number
add delta (Interval ( a, b )) =
    Interval ( a + delta, b + delta )


{-| Subtract the given amount from an interval.

    Interval.from -1 5 |> Interval.subtract 3
    --> Interval.from -4 2

-}
subtract : number -> Interval number -> Interval number
subtract delta (Interval ( a, b )) =
    Interval ( a - delta, b - delta )


{-| Multiply an interval by a given value. Note that this will flip the order
of the interval's endpoints if the given value is negative.

    Interval.multiplyBy 5 (Interval.from 2 3)
    --> Interval.from 10 15

    Interval.multiplyBy -2 (Interval.from 2 3)
    --> Interval.from -6 -4

-}
multiplyBy : number -> Interval number -> Interval number
multiplyBy scale (Interval ( a, b )) =
    if scale >= 0 then
        Interval ( a * scale, b * scale )

    else
        Interval ( b * scale, a * scale )


{-| Divide an interval by a given value. Note that this will flip the order
of the interval's endpoints if the given value is negative.

    Interval.divideBy 2 (Interval.from 2 3)
    --> Interval.from 1 1.5

    Interval.divideBy -2 (Interval.from 2 3)
    --> Interval.from -1.5 -1

-}
divideBy : Float -> Interval Float -> Interval Float
divideBy divisor (Interval ( a, b )) =
    if divisor == 0 then
        Interval ( -1 / 0, 1 / 0 )

    else if divisor > 0 then
        Interval ( a / divisor, b / divisor )

    else
        Interval ( b / divisor, a / divisor )


{-| Shorthand for `multiplyBy 0.5`.
-}
half : Interval Float -> Interval Float
half (Interval ( a, b )) =
    Interval ( 0.5 * a, 0.5 * b )


{-| Shorthand for `multiplyBy 2`.
-}
twice : Interval number -> Interval number
twice (Interval ( a, b )) =
    Interval ( 2 * a, 2 * b )


{-| Add two intervals together.

    Interval.from 5 10
        |> Interval.plus (Interval.from 2 3)
    --> Interval.from 7 13

-}
plus : Interval number -> Interval number -> Interval number
plus (Interval ( a2, b2 )) (Interval ( a1, b1 )) =
    Interval ( a2 + a1, b2 + b1 )


{-| Subtract the first interval from the second. This means that `minus` makes
the most sense when using `|>`:

    Interval.from 5 10
        |> Interval.minus (Interval.from 2 3)
    --> Interval.from 2 8

Without the pipe operator, the above would be written as:

    Interval.minus (Interval.from 2 3)
        (Interval.from 5 10)
    --> Interval.from 2 8

-}
minus : Interval number -> Interval number -> Interval number
minus (Interval ( a2, b2 )) (Interval ( a1, b1 )) =
    Interval ( a1 - b2, b1 - a2 )


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

            ( a, b ) =
                endpoints interval

            newMin =
                if includesMin then
                    -1

                else
                    min (Basics.sin a) (Basics.sin b)

            newMax =
                if includesMax then
                    1

                else
                    max (Basics.sin a) (Basics.sin b)
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

            ( a, b ) =
                endpoints interval

            newMin =
                if includesMin then
                    -1

                else
                    min (Basics.cos a) (Basics.cos b)

            newMax =
                if includesMax then
                    1

                else
                    max (Basics.cos a) (Basics.cos b)
        in
        fromEndpoints ( newMin, newMax )


{-| cos(x - pi/2) = sin(x), therefore if cos(interval - pi/2) includes
the maximum/minimum, that means sin(interval) includes the maximum/minimum
accordingly.
-}
sinIncludesMinMax : Interval Float -> ( Bool, Bool )
sinIncludesMinMax interval =
    interval |> subtract (pi / 2) |> cosIncludesMinMax


{-| cos(x + pi) = -cos(x), therefore if cos(interval + pi) includes the maximum,
that means cos(interval) includes the minimum.
-}
cosIncludesMinMax : Interval Float -> ( Bool, Bool )
cosIncludesMinMax interval =
    ( interval |> add pi |> cosIncludesMax
    , interval |> cosIncludesMax
    )


{-| The maximum of cos(x) is x = 2 pi \* k for every integer k.
If `minValue` and `maxValue` are in different branches
(meaning diffrent values of k), then the interval must pass through
2 pi \* k, which means the interval must include the maximum value.
-}
cosIncludesMax : Interval Float -> Bool
cosIncludesMax (Interval ( a, b )) =
    let
        minBranch =
            floor (a / (2 * pi))

        maxBranch =
            floor (b / (2 * pi))
    in
    minBranch /= maxBranch
