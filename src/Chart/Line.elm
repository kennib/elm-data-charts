module Chart.Line (polyline) where

import String

polyline : List (Float, Float) -> String
polyline points = String.join " "
    <| List.map (\(x,y) -> toString x ++ "," ++ toString y)
    <| points
