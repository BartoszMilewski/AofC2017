module Spiral where

{-
Coordinate system starts at square (0, 0)
Cells are numbered from 0 (rather than 1 as in the problem)

Notice that cell number (2n + 1)^2 has coordinates (n + 1, -n)
The next 4(2n+2) cells circle around the square with corners 
(-n, -n) and (n, n), which contains (2n+1)^2 cells
Each side adds 2n+2 cells
Indeed:
  (2(n+1)+1)^2 = (2n+1+2)^2 = (2n+1)^2 + 4(2n+2)

For instance, for n = 1, cell number 9 is at (2, -1)
on the outside of the full square with corners (-1, -1) (1, 1)
-}

-- Calculate the x coordinate of the corner
-- of the full square bounded by cell at given location
cornerX :: Int -> Int
cornerX loc = 
  -- take the square root
  let r = truncate $ sqrt $ fromIntegral loc
      -- make it odd: (2n + 1)
      n2 = if even r then r - 1 else r
      n  = truncate $ (fromIntegral n2) / 2
  in n

{--
 The spiral path, after filling the largest possible square,
 starts from (n+1, -n)
 with 2n + 2 squares on each side
-}
  
getPos :: Int -> (Int, Int)
getPos loc =
  let n = cornerX loc
      -- subtract the size of the square inside
      dist = loc - (2 * n + 1)^2
      sideLen = fromIntegral (2 * n + 2)
      -- which side?
      sideN = truncate $ (fromIntegral dist) / sideLen
      -- how many on this side?
      delta = dist - sideN * sideLen
  in  case sideN of
      0 -> (n + 1,      -n + delta) -- move up
      1 -> (n - delta,   n + 1)     -- move left
      2 -> (-n - 1,      n - delta) -- move down
      3 -> (-n + delta, -n - 1)     -- move right

-- Manhattan distance of cell at location loc
dist :: Int -> Int
dist loc = let (x, y) = getPos (loc - 1) -- adjust to zero-based
           in (abs x) + (abs y)
       
main :: IO ()
main = do
  print $ fmap dist [12, 23, 1024, 347991]
  -- [3, 2, 31, 480]
