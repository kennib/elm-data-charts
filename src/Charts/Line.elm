module Charts.Line (chart) where

import String

import Svg exposing (Svg, svg, g, polyline)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis, numberLabels, categoricalLabels)
import Chart.Scale as Scale
import Chart.Line as Line

chart : Layout -> List (Categorical a) -> List (List (Number b, Number b)) -> Svg
chart layout categories points = let
        xs = List.map fst <| List.concat points
        ys = List.map snd <| List.concat points
        axisWidth = Maybe.withDefault 0 <| Maybe.map .width  layout.axis
        axisHeight = Maybe.withDefault 0 <| Maybe.map .height layout.axis
    in
        svg
            [Html.style <| chartStyle layout.width layout.height]
            [ axis Axis.Left layout {data = ys, tickLabels = numberLabels}
            , axis Axis.Bottom layout {data = xs, tickLabels = numberLabels}
            , lines layout categories points
            ] 

chartStyle : Float -> Float -> List (String, String)
chartStyle width height =
    [ ("width", toString width)
    , ("height", toString height)
    ]

lines : Layout -> List (Categorical a) -> List (List (Number b, Number b)) -> Svg
lines layout categories points = let
        xs = List.map fst <| List.concat points
        ys = List.map snd <| List.concat points

        axisWidth = Maybe.withDefault 0 <| Maybe.map .width layout.axis
        axisHeight = Maybe.withDefault 0 <| Maybe.map .height layout.axis
        xAxisSize = layout.width  - axisWidth * 2
        xAxisMargin = axisWidth
        yAxisSize = layout.height - axisHeight * 2
        yAxisMargin = axisHeight
        xScale = Scale.number xs (xAxisMargin, xAxisMargin + xAxisSize)
        yScale = Scale.number ys (0, yAxisSize)
        scale (x,y) = (Maybe.withDefault 0 <| xScale x, Maybe.withDefault 0 <| yScale y)

        colors = List.map (Maybe.withDefault "grey" << Scale.categoryColors categories) categories
        points' = List.map (List.map scale) points
    in
        g []
        <| List.map2 line
            colors points'

line : String -> List (Float, Float) -> Svg
line color points = g []
    <| mapJoining (linePart color) points

linePart : String -> (Float, Float) -> (Float, Float) -> Svg
linePart color (x1', y1') (x2', y2') = Svg.line
    [ x1 <| toString x1'
    , y1 <| toString y1'
    , x2 <| toString x2'
    , y2 <| toString y2'
    , stroke color
    ] []

mapJoining : (a -> a -> b) -> List a -> List b
mapJoining f list = let
        tail = List.drop 1 list
    in
        List.map2 f list tail
