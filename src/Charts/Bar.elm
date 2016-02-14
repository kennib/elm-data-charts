module Charts.Bar (chart) where

import Svg exposing (Svg, svg, g, rect)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

import Chart.Data exposing (..)
import Chart.Axis as Axis exposing (axis)

chart : Int -> Int -> List (Categorical a) -> List (Number b) -> Svg
chart w h xs ys = let
        width = toFloat w
        height = toFloat h
        axisHeight = 40
        axisWidth = 60
    in
        svg
            [Html.style <| chartStyle width height]
            [ axis Axis.Left (height-axisHeight*2) axisWidth axisHeight <| NumberData ys
            , axis Axis.Bottom (width-axisWidth) axisHeight axisWidth <| CategoricalData xs
            , bars Vertical (width-axisWidth) (height-axisHeight) axisWidth <| List.map (\(Number y) -> y.number y.datum) ys
            ] 

chartStyle : Float -> Float -> List (String, String)
chartStyle width height =
    [ ("width", toString width)
    , ("height", toString height)
    ]

type BarOrientation = Vertical | Horizontal

bars : BarOrientation -> Float -> Float -> Float -> List Float -> Svg
bars orient size width pos values = let
        ticks = List.indexedMap (\i _ -> pos + (size * (toFloat i)/(toFloat <| List.length values))) values 
        barWidth = case ticks of
            pos0::pos1::_ -> pos1 - pos0
            _ -> width
    in
        g []
        <| List.map2 (bar orient size barWidth)
            ticks values

bar : BarOrientation -> Float -> Float -> Float -> Float -> Svg
bar orient size barWidth pos value = case orient of
    Vertical -> rect
        [ height <| toString value
        , width <| toString barWidth
        , x <| toString pos
        , y <| toString <| size - value
        ] []
    Horizontal -> rect
        [ width <| toString value
        , height <| toString barWidth
        , y <| toString pos
        ] []
