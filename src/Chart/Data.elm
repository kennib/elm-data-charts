module Chart.Data
    ( Data(..)
    , Number(..)
    , Categorical(..)
    , integer
    , categoricalString
    ) where

type Data a = NumberData (List (Number a)) | CategoricalData (List (Categorical a))

type Number a = Number
    { datum : a
    , number : a -> Float
    }

type Categorical a = Categorical
    { datum : a
    , label : a -> String
    }

integer : Int -> Number Int
integer x = Number
    { datum = x
    , number = toFloat
    }

categoricalString : String -> Categorical String
categoricalString x = Categorical
    { datum = x
    , label = identity
    }
