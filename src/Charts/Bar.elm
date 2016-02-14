module Charts.Bar (chart) where

import Svg exposing (Svg, svg, g, rect)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis)
import Chart.Scale as Scale

chart : Layout -> List (Categorical a) -> List (Number b) -> Svg
chart layout xs ys = let
        axisWidth = Maybe.withDefault 0 <| Maybe.map .width  layout.axis
        axisHeight = Maybe.withDefault 0 <| Maybe.map .height layout.axis
    in
        svg
            [Html.style <| chartStyle layout.width layout.height]
            [ axis Axis.Left layout <| NumberData ys
            , axis Axis.Bottom layout <| CategoricalData xs
            , bars Vertical layout xs ys
            ] 

chartStyle : Float -> Float -> List (String, String)
chartStyle width height =
    [ ("width", toString width)
    , ("height", toString height)
    ]

type BarOrientation = Vertical | Horizontal

bars : BarOrientation -> Layout -> List (Categorical a) -> List (Number b) -> Svg
bars orient layout xs ys = let
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
        xScale = Scale.categorical xs (xAxisMargin, xAxisMargin + xAxisSize)
        yScale = Scale.number ys (0, yAxisSize)

        ticks = List.filterMap xScale xs
        barHeights = List.filterMap yScale ys
        barWidth = case ticks of
            pos0::pos1::_ -> pos1 - pos0
            _ -> xAxisSize

        xPos = case orient of
            Vertical   -> ticks
            Horizontal -> List.repeat (List.length ys) <| yAxisMargin
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
