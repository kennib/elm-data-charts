import Chart
import Chart.Data as Data

import Mouse

main = Signal.map (Chart.bar Chart.defaultLayout labels)
    <| Signal.map (\(x, y) -> List.map Data.integer [0, x, y, 2000]) Mouse.position

labels : List (Data.Categorical String)
labels = List.map Data.categoricalString
    ["Zero", "X Position", "Y Position", "2K"]

