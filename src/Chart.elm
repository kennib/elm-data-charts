module Chart
    ( bar
    , line
    , area
    , defaultLayout
    ) where

import Chart.Data
import Chart.Layout

import Charts.Bar
import Charts.Line
import Charts.Area

bar = Charts.Bar.chart
line = Charts.Line.chart
area = Charts.Area.chart

defaultLayout = Chart.Layout.defaultLayout
