module Chart
    ( bar
    , defaultLayout
    ) where

import Chart.Data
import Chart.Layout
import Charts.Bar

bar = Charts.Bar.chart

defaultLayout = Chart.Layout.defaultLayout
