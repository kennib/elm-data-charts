module Chart.Axis (Orientation(..), axis) where

import List.Extra as List
import Svg exposing (Svg, svg, g, text', text)
import Svg.Attributes exposing (..)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Scale as Scale

type Orientation = Left | Right | Top | Bottom

axis : Orientation -> Layout -> Data a -> Svg
axis orient layout data = let
        labelTicks = dataLabels axisMargin axisSize data
        labels = List.map fst labelTicks
        ticks = List.map snd labelTicks

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

dataLabels : Float -> Float -> Data a -> List (String, Float)
dataLabels pos size data = case data of
    NumberData values -> numberLabels pos size values
    CategoricalData values -> categoricalLabels pos size values

numberLabels : Float -> Float -> List (Number a) -> List (String, Float)
numberLabels pos size data = let
        scale value = Maybe.withDefault 0 <| Scale.number data (pos, pos+size) value
        min = List.minimumBy (\(Number x) -> x.number x.datum) data
        max = List.maximumBy (\(Number x) -> x.number x.datum) data
    in
        case (min, max) of
            (Just min, Just max) -> List.map (\(Number x) -> (toString <| x.number x.datum, scale <| Number x)) [min, max]
            _ -> []

categoricalLabels : Float -> Float -> List (Categorical a) -> List (String, Float)
categoricalLabels pos size data = let
        scale value = Maybe.withDefault 0 <| Scale.categorical data (pos, pos+size) value
    in
        List.map (\(Categorical x) -> (x.label x.datum, scale <| Categorical x)) data
