
module Compose
       where

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
infixl 9 .>
f .> g = g . f

($>) :: a -> (a -> b) -> b
infixl 0 $>
x $> f = f x

