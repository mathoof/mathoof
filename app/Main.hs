module Main (main) where

import Parser
import Intro



main :: IO ()
main =  do
  content <- getContents
  mapM_ (print . runParser expression) $ lines content
