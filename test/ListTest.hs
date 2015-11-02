module ListTest where

li = do
  x <- [1,2,3]
  y <- [x, x+3, x+6]
  return (x,y)
