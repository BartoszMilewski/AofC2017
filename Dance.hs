module Dance where

import Data.List
import Data.List.Split

data Move = Spin Int 
          | Xchange Int Int 
          | Change Char Char 
            deriving Show

execMove :: Int -> String -> Move -> String
execMove len s (Spin n) = 
  drop (len - n) s ++ take (len - n) s
execMove _ s (Xchange m n) = 
  let (m', n') = (min m n, max m n)
      (l1, (a:l2)) = splitAt m' s
      (l3, (b:l4)) = splitAt (n' - m' - 1) l2
  in l1 ++ [b] ++ l3 ++ [a] ++ l4
execMove _ s (Change c d) = 
  let l = (split . oneOf) [c, d] s
  in head l ++ l !! 3 ++ l !! 2 ++ l !! 1 ++ l !! 4

run :: [Move] -> String -> String
run ms s = 
  let len = length s
  in foldl' (execMove len) s ms

main :: IO ()
main = do
  --print $ run (parse test) ['a'..'e']
  input <- readFile "Data.txt"
  print $ run (parse input) ['a'..'p']

parse :: String -> [Move]
parse = fmap parseMove . splitOn ","

parseMove (c: cs) = 
  case c of
    's' -> Spin $ read cs
    'p' -> let (x : y : _) = splitOn "/" cs
           in Change (head x) (head y)
    'x' -> let (x : y : _) = splitOn "/" cs
           in Xchange (read x) (read y)

test :: String
test = "s1,x3/4,pe/b"
