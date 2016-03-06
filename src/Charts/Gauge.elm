module Charts.Gauge (chart) where

import String

import Svg exposing (Svg, svg, g, path)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Layout exposing (..)
import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis, numberLabels, categoricalLabels)
import Chart.Scale as Scale
import Chart.Shape as Shape

chart : Layout -> Categorical a -> (Number b, Number b) -> Number b -> Svg
chart layout category (minValue, maxValue) value = let
        x = layout.width/2
        y = layout.height

        outerRadius = Basics.min layout.width layout.height / 2
        innerRadius = outerRadius - 50
        
        color = Maybe.withDefault "black" <| Scale.categoryColors [category] category

        gaugeScale = Scale.number [minValue, maxValue] (180, 0)
        gaugeStart = Maybe.withDefault 0 <| gaugeScale minValue 
        gaugeValue = Maybe.withDefault 0 <| gaugeScale value
        gaugeEnd = Maybe.withDefault 0 <| gaugeScale maxValue
    in
        svg
            [Html.style <| chartStyle layout.width layout.height]
            [ Svg.path [fill "lightgrey", d <| Shape.arc x y gaugeStart gaugeEnd innerRadius outerRadius] []
            , Svg.path [fill color, d <| Shape.arc x y gaugeStart gaugeValue innerRadius outerRadius] []
            ]

chartStyle : Float -> Float -> List (String, String)
chartStyle width height =
    [ ("width", toString width)
    , ("height", toString height)
    ]
