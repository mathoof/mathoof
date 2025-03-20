module Main where

import Intro
main :: IO ()
main = do
  content <- getContents
  mapM_ (print . simplify . defaultParser) $ lines content
