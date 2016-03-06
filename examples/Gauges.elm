import Chart
import Chart.Data as Data

import Time
import Window
import Mouse

main = Signal.map2 (\width x -> Chart.gauge Chart.defaultLayout label (Data.integer 0, Data.integer width) (Data.integer x))
    Window.width Mouse.x

label : Data.Categorical String
label = Data.categoricalString "Mouse X"
