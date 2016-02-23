import Chart
import Chart.Data as Data

import Time
import Mouse

main = Signal.map (Chart.area Chart.defaultLayout labels)
    <| Signal.map (\points -> [points])
    <| Signal.foldp (::) []
    <| Signal.map (\(t, y) -> (Data.float t, Data.float <| toFloat y))
    <| Time.timestamp Mouse.y

labels : List (Data.Categorical String)
labels = List.map Data.categoricalString
    ["Mouse Position"]
