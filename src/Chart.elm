module Chart
    ( bar
    , line
    , area
    , gauge
    , defaultLayout
    ) where

import Chart.Data
import Chart.Layout

import Charts.Bar
import Charts.Line
import Charts.Area
import Charts.Gauge

bar = Charts.Bar.chart
line = Charts.Line.chart
area = Charts.Area.chart
gauge = Charts.Gauge.chart

defaultLayout = Chart.Layout.defaultLayout
