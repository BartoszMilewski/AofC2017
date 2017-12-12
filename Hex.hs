module Hex where

import Data.List.Split
import Data.Maybe
import Data.List

{- 
  http://adventofcode.com/2017/day/11 

  Use 3-d system of coordinates:
  
    z |
      | 
     / \ 
  y /   \ x
  
  In this projection, a unit cube looks like a hexagon:
  
       |
      /|\
     |\|/|
     |/|\|
     /\|/\
    /     \
  Notice that, in this projection, point (0, 0, 0)
  coincides with (1, 1, 1). In fact it coincides
  with all points (n, n, n) for any positive or negative n.
  
  In general, (x, y, z) coincides with all (x+n, y+n, z+n)
  
-}

type Vec = (Int, Int, Int)

-- Points related by a shift are identified

shift :: Vec -> Int -> Vec
shift (x, y, z) d = (x - d, y - d, z - d)

data Dir = N | S | NW | NE | SW | SE
  deriving Show

-- Use Manhattan norm

norm :: Vec -> Int 
norm (x, y, z) = abs x + abs y + abs z

{-
  Given vector v, there are infinitely
  many equivalent shifted vectors, 
  but they all have different norms. 
  We want to find the one with 
  minimum norm.
 
  Of the three coordinates, one is of middle size.
  Shift the vector by that size. 
  The middle coordinate will become zero, 
  one will become non-negative, and one non-positive.
  Any shift by one from that position will increase the norm,
  because two absolute values will increase, 
  while only one will decrease. So it is a minimum.
-}

minimize v@(x, y, z) = 
  let mid = sort [x, y, z] !! 1
  in norm $ shift v mid

-- Translate directions to shifts in 3-d
move :: Vec -> Dir -> Vec
move (x, y, z) dir =
  case dir of
    N  -> (x,     y,     z + 1)
    S  -> (x,     y,     z - 1)
    SW -> (x,     y + 1, z    )
    NE -> (x,     y - 1, z    )
    SE -> (x + 1, y,     z    )
    NW -> (x - 1, y,     z    )

-- Parse directions
toDir :: String -> Dir
toDir (c:cs) = 
  case c of
    'n' -> case listToMaybe cs of
             Just 'w' -> NW
             Just 'e' -> NE
             Nothing -> N
    's' -> case listToMaybe cs of
             Just 'w' -> SW
             Just 'e' -> SE
             Nothing -> S

main :: IO ()
main = do 
  -- let input = test4
  input <- readFile "Data.txt"
  let dirs = fmap toDir $ splitOn "," input
      v = foldl' move (0, 0, 0) dirs
  print v
  print $ minimize v
  -- 664
  
test1 = "ne,ne,ne"
test2 = "ne,ne,sw,sw"
test3 = "ne,ne,s,s"
test4 = "se,sw,se,sw,sw"
