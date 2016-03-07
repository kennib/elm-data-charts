module Chart.Shape (area, arc) where

import Chart.Line as Line

import String

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

arc : Float -> Float -> Float -> Float -> Float -> Float -> String
arc x y startAngle endAngle innerRadius outerRadius = let
        (odx, ody) = fromPolar (outerRadius, degrees startAngle)
        (odx', ody') = fromPolar (outerRadius, degrees endAngle)
        outerx = x + odx
        outery = y - ody 
        outerdx = x + odx'
        outerdy = y - ody'

        (dx, dy) = fromPolar (innerRadius, degrees startAngle)
        (dx', dy') = fromPolar (innerRadius, degrees endAngle)
        innerx = x + dx
        innery = y - dy 
        innerdx = x + dx'
        innerdy = y - dy'

        largeArc = abs (endAngle - startAngle) > 180
        sweep = startAngle > endAngle

        start = Line.moveTo outerx outery
        outer = Line.arc outerRadius outerRadius 0 outerdx outerdy largeArc sweep
        line =  Line.line innerdx innerdy
        inner = Line.arc innerRadius innerRadius 0 innerx innery largeArc (not sweep)
        end = Line.line outerx outery
    in
        String.join " " [start, outer, line, inner, end]
