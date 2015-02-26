module Main where

import Data.Maybe
import System.Environment (getArgs)

import Brainfuck

main :: IO ()
main = do
  c <- getArgs >>= readFile . head
  interact $ fromMaybe "Unexpected end of input" . executeString c
