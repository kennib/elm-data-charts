module Chart.Axis (Orientation(..), axis) where

import Svg exposing (Svg, svg, g, text', text)
import Svg.Attributes exposing (..)

type Orientation = Left | Right | Top | Bottom

axis : Orientation -> Float -> Float -> Float -> List String -> Svg
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
