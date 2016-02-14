import Chart
import Chart.Data as Data

import Mouse

main = let
        width = 500
        height = 500
    in
           Signal.map (Chart.bar width height labels)
        <| Signal.map (\(x, y) -> List.map Data.integer [0, x, y, 2000]) Mouse.position

labels : List (Data.Categorical String)
labels = List.map Data.categoricalString
    ["Zero", "X Position", "Y Position", "2K"]

