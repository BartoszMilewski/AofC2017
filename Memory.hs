module Memory where
 
import Data.List
import qualified Data.HashSet as Set
import Data.Hashable

findMax :: Ord a => [a] -> Int
findMax as = snd $ firstMaxBy f (zip as [0..])
  where f (a, i) (a', i') = compare a a'

firstMaxBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
firstMaxBy cmp as = foldr1 mx as
  where mx n a = case cmp n a of
                   LT -> a
                   GT -> n
                   EQ -> n

data Zip a = Zip ![a] ![a]
  deriving Show

mkZip :: Int -> [a] -> Zip a
mkZip n as = Zip (reverse $ take n as) (drop n as)

unZip :: Zip a -> [a]
unZip (Zip b f) = reverse b ++ f

fwd :: Zip a -> Zip a
fwd (Zip b f) = Zip (head f : b) (tail f)

pull :: Zip Int -> (Int, Zip Int)
pull (Zip b f) = (head f, Zip b (0 : drop 1 f))

normalize :: Zip a -> Zip a
normalize z@(Zip b f) =
  if null f
  then Zip [] (reverse b)
  else z

distribute :: [Int] -> [Int]
distribute as =
    let i = findMax as
        z = mkZip i as
        (a, z') = pull z
        z'' = go a (fwd z')
    in unZip z''
  where 
    go a z = 
      if a == 0
      then z
      else go (a - 1) (inc z)
    inc z = 
      let (Zip b f) = normalize z
      in Zip (head f + 1 : b) (tail f)

type MemSet = Set.HashSet [Int]

findCycle :: [Int] -> Int -> MemSet -> (Int, MemSet)
findCycle as n set =
  let as' = distribute as
  in if Set.member as' set 
     then (n, set)
     else findCycle as' (n + 1) (Set.insert as' set)
      
main :: IO ()
main = do
  print $ findCycle input 1 Set.empty 

test, input :: [Int]
test = [0, 2, 7, 0]
input = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]
