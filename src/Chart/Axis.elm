module Chart.Axis (Orientation(..), axis) where

import Svg exposing (Svg, svg, g, text', text)
import Svg.Attributes exposing (..)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)

type Orientation = Left | Right | Top | Bottom

axis : Orientation -> Layout -> Data a -> Svg
axis orient layout data = let
        labels = dataLabels data
        numTicks = case data of
            NumberData _ -> List.length labels - 1
            CategoricalData _ -> List.length labels
        ticks = List.indexedMap (\i _ -> axisMargin + (axisSize * (toFloat i)/toFloat numTicks)) labels

        textSize = axisWidth / 4
        textAlign = case orient of
            Left   -> "end"
            Right  -> "start"
            Top    -> "start"
            Bottom -> "start"

        axisWidth = case orient of
            Left   -> Maybe.withDefault 0 <| Maybe.map .width  layout.axis
            Right  -> Maybe.withDefault 0 <| Maybe.map .width  layout.axis
            Top    -> Maybe.withDefault 0 <| Maybe.map .height layout.axis
            Bottom -> Maybe.withDefault 0 <| Maybe.map .height layout.axis
        axisMargin = case orient of
            Left   -> Maybe.withDefault 0 <| Maybe.map .height layout.axis
            Right  -> Maybe.withDefault 0 <| Maybe.map .height layout.axis
            Top    -> Maybe.withDefault 0 <| Maybe.map .width  layout.axis
            Bottom -> Maybe.withDefault 0 <| Maybe.map .width  layout.axis
        axisSize = case orient of
            Left   -> layout.height - axisMargin * 2
            Right  -> layout.height - axisMargin * 2
            Top    -> layout.width - axisMargin * 2
            Bottom -> layout.width - axisMargin * 2

        axisPos = List.repeat (List.length labels)
            <| case orient of
                Left   -> axisWidth
                Right  -> layout.width - axisWidth
                Top    -> axisWidth * 3/4
                Bottom -> layout.height - axisWidth * 3/4

        xPos = case orient of
            Left   -> axisPos
            Right  -> axisPos
            Top    -> ticks
            Bottom -> ticks 
        yPos = case orient of
            Left   -> List.reverse ticks
            Right  -> List.reverse ticks
            Top    -> axisPos
            Bottom -> axisPos
    in
        g []
        <| List.map3 (axisLabel textAlign textSize)
            xPos yPos labels

axisLabel : String -> Float -> Float -> Float -> String -> Svg
axisLabel align size xPos yPos label = text'
    [ x <| toString xPos
    , y <| toString yPos
    , fontSize <| toString size
    , textAnchor align
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
