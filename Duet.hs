{-
  http://adventofcode.com/2017/day/18
-}

module Duet where
  
import Data.Char
import Data.List
import qualified Data.Map as Map
import Debug.Trace

type Reg = Char

-- Safe zipper with jump functionality
data Zipper a = Zip ![a] ![a]
  deriving Show

zCur :: Zipper a -> Maybe a
zCur (Zip ls (r:rs)) = Just r
zCur _ = Nothing

zNext :: Zipper a -> Maybe (Zipper a)
zNext (Zip ls (r:rs)) = Just $ Zip (r:ls) rs
zNext _ = Nothing

zJump :: Int -> Zipper a -> Maybe (Zipper a)
zJump n z = 
    if n >= 0 then zFwd n z else zBck n z
  where
    zFwd n (Zip ls rs) =
      if n > length rs
      then Nothing
      else Just $ Zip (reverse (take n rs) ++ ls) (drop n rs)
    zBck n (Zip ls rs) =
      if -n > length ls
      then Nothing
      else Just $ Zip (drop (-n) ls) (reverse (take (-n) ls) ++ rs)

-- Since we most often execute sequential instructions
-- we store program as a zipper, for quick access to 
-- current instruction and quick advancement
data Processor = Proc {
  regs    :: Map.Map Reg Int, -- register file
  freq    :: Int,             -- last played frequency
  prog    :: Zipper Instr     -- program instructions
  } deriving Show
  
newProcessor :: [Instr] -> Processor
newProcessor prg = Proc Map.empty 0 (Zip [] prg)

jump :: Int -> Processor -> Maybe Processor
jump n p = case zJump n (prog p) of
             Nothing -> Nothing
             Just z' -> Just $ p { prog = z' }

type Arg = Either Reg Int

getVal :: Arg -> Processor -> Int
getVal (Left r) p = getReg r p
getVal (Right n) _ = n
    
getReg :: Reg -> Processor -> Int
getReg r p =
  case Map.lookup r (regs p) of
    Nothing -> 0
    Just n -> n
    
setReg :: Reg -> Int -> Processor -> Processor
setReg r n p = p { regs = Map.insert r n (regs p) }

-- Arithmetic instructions store the operator
-- as a binary function
data Instr = Arith Reg Arg (Int -> Int -> Int)
          | Jgz Arg  Arg
          | Snd Arg
          | Rcv Arg
          
instance Show Instr where
  show (Arith r a _) = "Arith " ++ [r] ++ " " ++ show a
  show (Jgz a1 a2) = "Jgz " ++ show a1 ++ " " ++ show a2
  show (Snd a) = "Snd " ++ show a
  show (Rcv a) = "Rcv " ++ show a

-- Execute one instruction
-- Left Nothing means we moved outside of program range
-- Left Just gives the last played frequency
-- Right means we should continue

exec1 :: Instr -> Processor -> Either (Maybe Int) Processor
exec1 (Arith r arg op) p =
  let v = getReg r p `op` getVal arg p
      regs' = Map.insert r v (regs p)
  in advance $ p { regs = regs' }

exec1 (Snd arg) p = 
  advance $ p { freq = getVal arg p }

exec1 (Rcv arg) p = 
  if getVal arg p == 0 
  then advance p
  else Left $ Just (freq p)

exec1 (Jgz arg1 arg2) p = 
  if getVal arg1 p <= 0 
  then advance p
  else case zJump (getVal arg2 p) (prog p) of
         Nothing -> Left Nothing
         Just z' -> Right $ p { prog = z' }
         
advance :: Processor -> Either (Maybe Int) Processor
advance p = 
  case zNext (prog p) of
        Nothing -> Left Nothing
        Just z' -> Right $ p { prog = z' }

-- Fetch and execute current instruction
exec :: Processor -> Either (Maybe Int) Processor
exec p = case zCur (prog p) of
  Nothing -> Left Nothing
  Just i -> exec1 i p

-- Run the whole program
run :: Processor -> Either (Maybe Int) Processor
run p = case exec p of
  Left x -> Left x
  Right p -> run p -- recurse

main :: IO ()
main = do
  input <- readFile "Data.txt"
  let prog = fmap (parseInstr . words) (lines input)
      proc = newProcessor prog
  case run proc of
        Left Nothing -> print "Program failed"
        Left (Just f) -> print f
        Right p -> print p

-- Parsing the input

parseInstr :: [String] -> Instr
parseInstr (ins : a1 : t) =
    case ins of
      "add" -> Arith (toReg a1) (arg $ head t) (+)
      "mul" -> Arith (toReg a1) (arg $ head t) (*)
      "mod" -> Arith (toReg a1) (arg $ head t) mod
      "set" -> Arith (toReg a1) (arg $ head t) (flip const)
      "jgz" -> Jgz (arg a1) (arg $ head t)
      "snd" -> Snd (arg a1)
      "rcv" -> Rcv (arg a1)
  where
    toReg s = head s
    arg s = if isAlpha (head s) 
            then Left (head s) 
            else Right (read s)

test :: [String]
test = [
 "set a 1",
 "add a 2",
 "mul a a",
 "mod a 5",
 "snd a",
 "set a 0",
 "rcv a",
 "jgz a -1",
 "set a 1",
 "jgz a -2"]
 
