module Charts.Bar (chart) where

import Chart.Data exposing (..)
import Svg exposing (Svg, svg, g, rect, text', text)
import Svg.Attributes exposing (..)
import Html.Attributes as Html exposing (style)

chart : Int -> Int -> List (Categorical a) -> List (Number b) -> Svg
chart w h xs ys = let
        width = toFloat w
        height = toFloat h
        axisHeight = 40
        axisWidth = 60
        values = List.map (\(Number y) -> y.number y.datum) ys
    in
        svg
            [Html.style <| chartStyle width height]
            [ axis Left height axisWidth axisHeight <| List.map toString [Maybe.withDefault 0 <| List.minimum values, Maybe.withDefault 1 <| List.maximum values]
            , axis Bottom width axisHeight axisWidth <| List.map (\(Categorical x) -> x.label x.datum) xs
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

type AxisOrientation = Left | Right | Top | Bottom

axis : AxisOrientation -> Float -> Float -> Float -> List String -> Svg
axis orient size width pos labels = let
        textSize = width/4
        sides = List.repeat (List.length labels)
            <| case orient of
                Left -> 0
                Right -> size - width
                Top -> 0
                Bottom -> size - width
        ticks = List.indexedMap (\i _ -> pos + (size * (toFloat i)/(toFloat <| List.length labels))) labels 
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
    [x <| toString xPos, y <| toString yPos, fontSize <| toString size]
    [text label]
