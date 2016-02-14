module Chart.Axis (Orientation(..), axis) where

import Svg exposing (Svg, svg, g, text', text)
import Svg.Attributes exposing (..)

import Chart.Data exposing (..)

type Orientation = Left | Right | Top | Bottom

axis : Orientation -> Float -> Float -> Float -> Data a -> Svg
axis orient size width pos data = let
        textSize = width/4
        labels = dataLabels data
        sides = List.repeat (List.length labels)
            <| case orient of
                Left -> 0
                Right -> size
                Top -> width/2
                Bottom -> size + width/2
        numTicks = case data of
            NumberData _ -> List.length labels - 1
            CategoricalData _ -> List.length labels
        ticks = List.indexedMap (\i _ -> pos + (size * (toFloat i)/toFloat numTicks)) labels
        xPos = case orient of
            Left -> sides
            Right -> sides
            Top -> ticks
            Bottom -> ticks 
        yPos = case orient of
            Left -> List.reverse ticks
            Right -> List.reverse ticks
            Top -> sides
            Bottom -> sides
    in
        g []
        <| List.map3 (axisLabel textSize)
            xPos yPos labels

axisLabel : Float -> Float -> Float -> String -> Svg
axisLabel size xPos yPos label = text'
    [ x <| toString xPos
    , y <| toString yPos
    , fontSize <| toString size
    ] [text label]

dataLabels : Data a -> List String
dataLabels data = case data of
    NumberData values -> numberLabels values
    CategoricalData values -> categoricalLabels values

numberLabels : List (Number a) -> List String
numberLabels data = let
        values = List.map (\(Number x) -> x.number x.datum) data
        min = Maybe.withDefault 0 <| List.minimum values
        max = Maybe.withDefault 1 <| List.maximum values
    in
        List.map toString [min, max]

categoricalLabels : List (Categorical a) -> List String
categoricalLabels data = List.map (\(Categorical x) -> x.label x.datum) data
