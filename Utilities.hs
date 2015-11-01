module Utilities where

--how to make point-free?
c2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
c2 f g x y = f $ g x y
