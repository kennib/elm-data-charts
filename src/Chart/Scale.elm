module Chart.Scale (linear, ordinal, quantile, number, categorical, ordinalCategories, categoryColors) where

import List.Extra as List

import Chart.Data as Data exposing (Data(..), Number(..), Categorical(..))

type alias Range a = (a, a)

linear : Range Float -> Range Float -> Float -> Maybe Float
linear (domainMin, domainMax) (rangeMin, rangeMax) value = let
        rangeValue = rangeMin + (rangeMax - rangeMin) * (value - domainMin) / (domainMax - domainMin)
    in
        if domainMax == domainMin then
            Nothing
        else if value >= min domainMin domainMax && value < max domainMin domainMax then
            Just rangeValue
        else
            Just rangeValue

quantile : Range Float -> Range Int -> Int -> Float -> Maybe Int
quantile (domainMin, domainMax) (rangeMin, rangeMax) delta value = let
        scale = linear (domainMin, domainMax) (toFloat rangeMin, toFloat rangeMax)
    in
        case scale value of
            Just rangeValue -> Just <| ((floor <| rangeValue) // delta) * delta
            Nothing -> Nothing

ordinal : List a -> Range Float -> a -> Maybe Float
ordinal domain range value = let
        index = Maybe.map toFloat <| List.elemIndex value domain
        domainRange = (0, toFloat <| List.length domain)
    in
        index `Maybe.andThen` linear domainRange range

number : List (Number a) -> Range Float -> Number a -> Maybe Float
number numbers range (Number x) = let
        domain = listRange <| List.map (\(Number x) -> x.number x.datum) numbers 
        value = x.number x.datum
    in
        domain `Maybe.andThen` \domain -> linear domain range value 

categorical : List (Categorical a) -> Range Float -> Categorical a -> Maybe Float
categorical categories range category = ordinal categories range category

ordinalCategories : List (Categorical a) -> List (Categorical b) -> Categorical a -> Maybe (Categorical b)
ordinalCategories domain range value =
    List.elemIndex value domain
        `Maybe.andThen`
    \index -> List.getAt range (index % List.length range)

categoryColors : List (Categorical a) -> Categorical a -> Maybe String
categoryColors categories = let
        colorOf (Categorical x) = x.label x.datum
    in
        Maybe.map colorOf <<
            (ordinalCategories categories
            <| List.map Data.categoricalString colors)

colors : List String
colors = ["red", "green", "blue", "gold", "salmon", "silver", "purple"]

listRange : List comparable -> Maybe (Range comparable)
listRange list = let
        min = List.minimum list
        max = List.maximum list
    in
        Maybe.map2 (\min max -> (min, max))
            min max
