module Chart.Data
    ( Data
    , DataType(..)
    , Number
    , Categorical
    , float
    , integer
    , categoricalString
    ) where

type alias Data a =
    { data : List a
    , kind : DataType
    }

type DataType = NumberData | CategoricalData

type alias Number a =
    { datum : a
    , number : a -> Float
    }

type alias Categorical a =
    { datum : a
    , label : a -> String
    }

float : Float -> Number Float
float x =
    { datum = x
    , number = identity
    }

integer : Int -> Number Int
integer x =
    { datum = x
    , number = toFloat
    }

categoricalString : String -> Categorical String
categoricalString x =
    { datum = x
    , label = identity
    }
