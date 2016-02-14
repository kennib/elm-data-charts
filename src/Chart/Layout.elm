module Chart.Layout (Layout, defaultLayout) where

type alias Layout =
    { width : Float
    , height : Float
    , axis : Maybe AxisLayout
    }

type alias AxisLayout =
    { width : Float
    , height : Float
    }

defaultLayout =
    { width = 500
    , height = 500
    , axis = Just { width = 60, height = 40 }
    }
