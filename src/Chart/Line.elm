module Chart.Line (polyline, area) where

import String

polyline : List (Float, Float) -> String
polyline points = String.join " "
    <| List.map (\(x,y) -> toString x ++ "," ++ toString y)
    <| points

area : Float -> List (Float, Float) -> String
area baseline points = let
        firstX = Maybe.withDefault 0 <| Maybe.map fst <| List.head points
        lastX = Maybe.withDefault 0 <| Maybe.map fst <| List.head <| List.drop (List.length points - 1) points
        point (x,y) = toString x ++ " " ++ toString y
        start = "M " ++ point (firstX, baseline)
        end = point (lastX, baseline) ++ " Z"
        points' = String.join " L "
            <| List.map point
            <| points
    in
        if points /= [] then
            start ++ " L " ++ points' ++ " L " ++ end
        else
            ""
