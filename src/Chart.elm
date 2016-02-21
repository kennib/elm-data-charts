module Chart
    ( bar
    , line
    , defaultLayout
    ) where

import Chart.Data
import Chart.Layout

import Charts.Bar
import Charts.Line

bar = Charts.Bar.chart
line = Charts.Line.chart

defaultLayout = Chart.Layout.defaultLayout
