module Chart.Line (polyline, line, arc, moveTo) where

import String

polyline : List (Float, Float) -> String
polyline points = String.join " "
    <| List.map (\(x,y) -> toString x ++ "," ++ toString y)
    <| points

line : Float -> Float -> String
line x y = let
        positions = List.map toString [x, y]
    in
        "L " ++ String.join " " positions

arc : Float -> Float -> Float -> Float -> Float -> Bool -> Bool -> String
arc rx ry rotation x' y' largeArc sweep = let
        largeArcFlag = if largeArc then 1 else 0
        sweepFlag = if sweep then 1 else 0
    in
        String.join " "
        (  ["A"]
        ++ List.map toString [rx, ry, rotation, largeArcFlag, sweepFlag, x', y']
        )

moveTo : Float -> Float -> String
moveTo x y = let
        positions = List.map toString [x, y]
    in
        "M " ++ String.join " " positions
