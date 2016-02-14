import Chart
import Chart.Data as Data

import Random exposing (..)
import Char exposing (toCode, fromCode)

main = Chart.bar Chart.defaultLayout
    categories
    <| List.map Data.integer 
    <| fst <| generate randomValues <| initialSeed 0

categories : List (Data.Categorical String)
categories = List.map Data.categoricalString
    <| ["Circle", "Square", "Diamond", "Ellipse", "Arrow", "Cross", "Hexagon", "Triangle"]

randomValues : Generator (List Int)
randomValues = list 8 (int 0 100)
