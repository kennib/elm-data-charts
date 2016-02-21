module Charts.Line (chart) where

import String

import Svg exposing (Svg, svg, g, polyline)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis)
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
            [ axis Axis.Left layout <| NumberData ys
            , axis Axis.Bottom layout <| NumberData xs
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

        points' = List.map (List.map scale) points
    in
        g []
        <| List.map line
            points'

line : List (Float, Float) -> Svg
line points = g []
    <| mapJoining linePart points

linePart : (Float, Float) -> (Float, Float) -> Svg
linePart (x1', y1') (x2', y2') = Svg.line
    [ x1 <| toString x1'
    , y1 <| toString y1'
    , x2 <| toString x2'
    , y2 <| toString y2'
    , stroke "black"
    ] []

mapJoining : (a -> a -> b) -> List a -> List b
mapJoining f list = let
        tail = List.drop 1 list
    in
        List.map2 f list tail
