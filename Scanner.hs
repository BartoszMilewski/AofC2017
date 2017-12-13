module Scanner where

import Data.List.Split

data Scanner = Scan {
  depth :: Int,
  range :: Int } deriving Show
  
-- Calculate the position of the scanner
-- when we are crossing it.
-- Each scanner goes through even and odd cycles

getPos :: Scanner -> Int
getPos sc = 
  let cycle = depth sc `div` (range sc - 1)
      dist  = depth sc `mod` (range sc - 1)
  in if even cycle 
     then dist
     else range sc - 1 - dist

total :: [Scanner] -> Int
total = sum . fmap score
  where
    score :: Scanner -> Int
    score sc = if getPos sc == 0 
               then depth sc * range sc 
               else 0

main :: IO ()
main = do
  --print $ parse test
  --print $ total $ parse test
  input <- readFile "Data.txt"
  let scs = parse (lines input)
  print $ total scs

parse :: [String] -> [Scanner]
parse = fmap parseLine
  where 
    parseLine :: String -> Scanner
    parseLine ln = 
      let (d : r : _) = (split . dropBlanks . dropDelims. oneOf) ": " ln
      in Scan (read d) (read r)

test :: [String]
test = [
 "0: 3",
 "1: 2",
 "4: 4",
 "6: 4"]
