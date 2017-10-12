module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let port = read (head args) :: Int
  startApp port
