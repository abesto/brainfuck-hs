module Brainfuck where

import Data.Char
import Data.Array.Unboxed
import Data.Map (Map)
import qualified Data.Map as Map


data State = State { mem :: UArray Int Int
                   , memPtr :: Int
                   , code :: UArray Int Char
                   , opPtr :: Int
                   , input :: String
                   , output :: String
                   , jmpMap :: (Map Int Int, Map Int Int)
                   , err :: Bool }
             deriving Show

initialState :: String -> String -> State
initialState s i = State { mem      = listArray (0, 1024) $ repeat 0  -- I strongly doubt anyone will ever need more than 1K of memory (famous last words)
                         , memPtr   = 512                             -- Start in the middle so that we can move either way
                         , code     = listArray (0, length s) s
                         , opPtr    = 0
                         , input    = i
                         , output   = ""
                         , jmpMap   = calcJmpMap s
                         , err      = False
                         }

execute :: Char -> State -> State

execute '>' s = modMemPtr (+) s

execute '<' s = modMemPtr (-) s

execute '+' s = modVal (\n -> if n == 255 then 0 else n + 1) s

execute '-' s = modVal (\n -> if n == 0 then 255 else n - 1) s

execute '.' s = s { output = (output s) ++ [chr $ val s]}

execute ',' s
  | null $ input s = s { err = True }
  | otherwise      = setVal (ord $ head $ input s) s { input = tail $ input s}

execute '[' s = if val s == 0
                    then s { opPtr = (fst $ jmpMap s) Map.! (opPtr s) }
                    else s

execute ']' s = if val s /= 0
                    then s { opPtr = (snd $ jmpMap s) Map.! (opPtr s) }
                    else s

execute _ s = s

modMemPtr :: (Int -> Int -> Int) -> State -> State
modMemPtr f s = s { memPtr = memPtr s `f` 1}

val :: State -> Int
val s = mem s ! memPtr s

setVal :: Int -> State -> State
setVal n s = s { mem = mem s // [(memPtr s, n)] }

modVal :: (Int -> Int) -> State -> State
modVal f s = setVal (f $ val s) s

run :: State -> State
run s =
  if opPtr s >= snd (bounds (code s)) || err s
     then s
     else run $ nextOp $ execute op s
  where
    op = code s ! opPtr s
    nextOp s' = s' { opPtr = opPtr s' + 1 }

calcDepth :: String -> Int -> [Int]
calcDepth ('[':s) n = n : (calcDepth s $ n + 1)
calcDepth (']':s) n = n : (calcDepth s $ n - 1)
calcDepth (_:s) n = n : (calcDepth s n)
calcDepth "" _ = []

calcJmpAssoc :: String -> [(Int, Int)]
calcJmpAssoc s = f [] $ zip3 (calcDepth s 0) ((tail $ calcDepth s 0) ++ [0]) [0..]
  where
    f _ [] = []
    f acc ((d, d', idx):ds)
          | d == d' = f acc ds
          | d < d' = f (idx:acc) ds
          | d > d' = (head acc, idx) : (f (tail acc) ds)
    f _ _ = error "Unexpected error. Maybe parenthesis nesting is incorrect in the Brainfuck code?"

calcJmpMap :: String -> (Map Int Int, Map Int Int)
calcJmpMap s = f (calcJmpAssoc s) (Map.empty, Map.empty)
  where f [] m = m
        f ((l, r):xs) (ml, mr) = f xs (Map.insert l r ml, Map.insert r l mr)

executeString :: String -> String -> Maybe String
executeString c i = f $ run $ initialState c i
  where f s = if err s then Nothing else Just $ output s
