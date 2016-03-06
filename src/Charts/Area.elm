module Charts.Area (chart) where

import Svg exposing (Svg, svg, g, path)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis, numberLabels, categoricalLabels)
import Chart.Scale as Scale
import Chart.Shape as Shape

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
            , areas layout categories points
            ] 

chartStyle : Float -> Float -> List (String, String)
chartStyle width height =
    [ ("width", toString width)
    , ("height", toString height)
    ]

areas : Layout -> List (Categorical a) -> List (List (Number b, Number b)) -> Svg
areas layout categories points = let
        xs = List.map fst <| List.concat points
        ys = List.map snd <| List.concat points

        axisWidth = Maybe.withDefault 0 <| Maybe.map .width layout.axis
        axisHeight = Maybe.withDefault 0 <| Maybe.map .height layout.axis
        xAxisSize = layout.width  - axisWidth * 2
        xAxisMargin = axisWidth
        yAxisSize = layout.height - axisHeight * 2
        yAxisMargin = axisHeight
        baseline = yAxisSize
        xScale = Scale.number xs (xAxisMargin, xAxisMargin + xAxisSize)
        yScale = Scale.number ys (0, yAxisSize)
        scale (x,y) = (Maybe.withDefault 0 <| xScale x, Maybe.withDefault 0 <| yScale y)

        colors = List.map (Maybe.withDefault "grey" << Scale.categoryColors categories) categories
        points' = List.map (List.map scale) points
    in
        g []
        <| List.map2 (area baseline)
            colors points'

area : Float -> String -> List (Float, Float) -> Svg
area baseline color points = Svg.path
    [ fill color
    , d <| Shape.area baseline points
    ] []
