{-# LANGUAGE DeriveFunctor #-}

module Bridge where
  
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Maybe

main = do
  input <- readFile "Data.txt"
  let pool = fmap toPiece (lines input)
  print $ bestChain pool

type Piece = (Int, Int)

type Chain = [Piece]

type Pool = Map.Map Int [Int]

addPiece :: Piece -> Pool -> Pool
addPiece (m, n) = if m /= n 
                  then add m n . add n m
                  else add m n
  where 
    add m n pool = 
      case Map.lookup m pool of
        Nothing  -> Map.insert m [n] pool
        Just lst -> Map.insert m (n : lst) pool

removePiece :: Piece -> Pool -> Pool
removePiece (m, n) = if m /= n
                     then rem m n . rem n m
                     else rem m n
  where
    rem :: Int -> Int -> Pool -> Pool
    rem m n pool = 
      case fromJust $ Map.lookup m pool of
        []  -> Map.delete m pool
        lst -> Map.insert m (delete n lst) pool

presort :: [Piece] -> Pool
presort = foldr addPiece Map.empty 
          
-- Tree of chains
-- Each node contains a port
-- and a list of smaller chains
-- Top node will contain port 0
data Rose = NodeR Int [Rose]
  deriving Show

-- Represent the tree as a fixed point of a functor

data TreeF a = NodeF Int [a]
  deriving Functor
  
newtype Fix f = Fix { unFix :: f (Fix f) }

type Tree = Fix TreeF

type Coalgebra f a = a -> f a

-- This coalgebra builds a tree using anamorphism

coalg :: Coalgebra TreeF (Int, Pool)
coalg (n, pool) = 
  case Map.lookup n pool of
    Nothing -> NodeF n []
    Just ms -> NodeF n [(m, removePiece (m, n) pool) | m <- ms]
    
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

type Algebra f a = f a -> a

-- This algebra is for testing

tAlg :: Algebra TreeF Rose
tAlg (NodeF n lst) = NodeR n lst

-- This algebra turns a tree into a list of chains
chainAlg :: Algebra TreeF (Int, [Chain])
chainAlg (NodeF n []) = (n, [])
chainAlg (NodeF n lst) = (n, concat [push (n, m) bs | (m, bs) <- lst])
  where
    push :: (Int, Int) -> [Chain] -> [Chain]
    push (n, m) [] = [[(n, m)]]
    push (n, m) bs = [(n, m) : br | br <- bs]

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

score :: Chain -> Int
score = sum . fmap score1
  where score1 (m, n) = m + n

bestChain :: [Piece] -> Int
bestChain pieces =
  let pool = presort pieces
      (_, chains) = hylo chainAlg coalg (0, pool)
  in maximum $ fmap score chains

toPiece :: String -> Piece
toPiece s = let (a : b : _) = splitOn "/" s
         in (read a, read b)
