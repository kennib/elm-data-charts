import Chart
import Chart.Data as Data

import Mouse

main = Signal.map (Chart.line Chart.defaultLayout labels)
    <| Signal.map (\points -> [points])
    <| Signal.foldp (\(x,y) list -> (Data.integer x, Data.integer y)::list) []
    <| Mouse.position

labels : List (Data.Categorical String)
labels = List.map Data.categoricalString
    ["Mouse Position"]
