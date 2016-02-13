import Chart
import Chart.Data as Data

import Random exposing (..)
import Char exposing (toCode, fromCode)

main = let
        width = 500
        height = 500
    in
        Chart.bar width height
        categories
        <| List.map Data.integer 
        <| fst <| generate randomValues <| initialSeed 0

categories : List (Data.Categorical String)
categories = List.map Data.categoricalString
    <| ["Circle", "Square", "Diamond", "Ellipse", "Arrow", "Cross", "Hexagon", "Triangle"]

randomValues : Generator (List Int)
randomValues = list 8 (int 0 100)
