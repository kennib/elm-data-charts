module Charts.Bar (chart) where

import Svg exposing (Svg, svg, g, rect)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis)

chart : Int -> Int -> List (Categorical a) -> List (Number b) -> Svg
chart w h xs ys = let
        width = toFloat w
        height = toFloat h
        layout = {defaultLayout | width = width, height = height}
        axisWidth = Maybe.withDefault 0 <| Maybe.map .width  layout.axis
        axisHeight = Maybe.withDefault 0 <| Maybe.map .height layout.axis
    in
        svg
            [Html.style <| chartStyle width height]
            [ axis Axis.Left layout <| NumberData ys
            , axis Axis.Bottom layout <| CategoricalData xs
            , bars Vertical layout <| List.map (\(Number y) -> y.number y.datum) ys
            ] 

chartStyle : Float -> Float -> List (String, String)
chartStyle width height =
    [ ("width", toString width)
    , ("height", toString height)
    ]

type BarOrientation = Vertical | Horizontal

bars : BarOrientation -> Layout -> List Float -> Svg
bars orient layout values = let
        min = Maybe.withDefault 0 <| List.minimum values
        max = Maybe.withDefault 1 <| List.maximum values
        axisWidth = Maybe.withDefault 0 <| Maybe.map .width  layout.axis
        axisHeight = Maybe.withDefault 0 <| Maybe.map .height layout.axis
        xAxisSize = case orient of
            Vertical   -> layout.width  - axisWidth * 2
            Horizontal -> layout.height - axisHeight * 2
        xAxisMargin = case orient of
            Vertical   -> axisWidth
            Horizontal -> axisHeight
        yAxisSize = case orient of
            Vertical   -> layout.height - axisHeight * 2
            Horizontal -> layout.width  - axisWidth * 2
        yAxisMargin = case orient of
            Vertical   -> axisHeight
            Horizontal -> axisWidth
        ticks = List.indexedMap (\i _ -> xAxisMargin + (xAxisSize * (toFloat i)/(toFloat <| List.length values))) values 
        barHeights = List.map (\value -> yAxisSize * (value - min) / (max - min)) values
        barWidth = case ticks of
            pos0::pos1::_ -> pos1 - pos0
            _ -> xAxisSize

        xPos = case orient of
            Vertical   -> ticks
            Horizontal -> List.repeat (List.length values) <| yAxisMargin
        yPos = case orient of
            Vertical   -> List.map (\value -> yAxisMargin + yAxisSize - value) barHeights
            Horizontal -> ticks
    in
        g []
        <| List.map3 (bar barWidth)
            xPos yPos barHeights

bar : Float -> Float -> Float -> Float -> Svg
bar barWidth xPos yPos value = rect
    [ height <| toString value
    , width <| toString barWidth
    , x <| toString xPos
    , y <| toString <| yPos
    ] []
