module Polynom where

type Polynom = [Double]
cmult :: Polynom -> Double -> Polynom
cmult p x = map (x*) p

eval :: Polynom -> Double -> Double
eval p x = foldr (\a b -> a + b * x) 0 p

deriv :: Polynom -> Polynom
deriv p = zipWith (*) [1..] (tail p)

