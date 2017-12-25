{-# LANGUAGE DeriveFunctor #-}

module Bridge where
  
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Maybe

type Part = (Int, Int)

type Bridge = [Part]

type Parts = Map.Map Int [Int]

addPart :: Part -> Parts -> Parts
addPart (m, n) = if m /= n 
                 then add m n . add n m
                 else add m n
  where 
    add m n box = 
      case Map.lookup m box of
        Nothing -> Map.insert m [n] box
        Just lst -> Map.insert m (n : lst) box

removePart :: Part -> Parts -> Parts
removePart (m, n) = if m /= n
                    then rem m n . rem n m
                    else rem m n
  where
    rem :: Int -> Int -> Parts -> Parts
    rem m n box = 
      case fromJust $ Map.lookup m box of
        []  -> Map.delete m box
        lst -> Map.insert m (delete n lst) box

presort :: [Part] -> Parts
presort = foldl' (flip addPart) Map.empty 
          
-- Tree of bridges
-- Each node contains a port
-- and a list of smaller bridges
-- Top node will contain port 0
data TreeB = Nd Int [TreeB]
  deriving Show

-- Represent the tree as a fixed point of a functor

data TreeF a = NodeF Int [a]
  deriving Functor
  
newtype Fix f = Fix { unFix :: f (Fix f) }

type Tree = Fix TreeF

type Coalgebra f a = a -> f a

-- This coalgebra builds a tree using anamorphism

coalg :: Coalgebra TreeF (Int, Parts)
coalg (n, box) = 
  case Map.lookup n box of
    Nothing -> NodeF 0 []
    Just ms -> NodeF n [(m, removePart (m, n) box) | m <- ms]
    
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

type Algebra f a = f a -> a

-- This algebra is for testing

tAlg :: Algebra TreeF TreeB
tAlg (NodeF n lst) = Nd n lst

-- This algebra turns a tree into a list of bridges
bridgAlg :: Algebra TreeF (Int, [Bridge])
bridgAlg (NodeF n []) = (n, [])
bridgAlg (NodeF n lst) = (n, concat [f (n, m) bs | (m, bs) <- lst])
  where
    f :: (Int, Int) -> [Bridge] -> [Bridge]
    f (n, m) [] = [[(n, m)]]
    f (n, m) bs = [(n, m) : br | br <- bs]

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

score :: Bridge -> Int
score = sum . fmap score1
  where score1 (m, n) = m + n

buildBridges :: [Part] -> Int
buildBridges parts =
  let box = presort parts
      -- t = ana coalg (0, box)
      -- (_, bridges) = cata bridgAlg t
      (_, bridges) = hylo bridgAlg coalg (0, box)
  in maximum $ fmap score bridges

main = do
  input <- readFile "Data.txt"
  let parts = fmap toPart (lines input)
  print $ buildBridges parts

toPart :: String -> Part
toPart s = let (a : b : _) = splitOn "/" s
         in (read a, read b)
 
