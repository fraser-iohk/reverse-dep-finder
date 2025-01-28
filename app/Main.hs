module Main where

import Options
import qualified ReverseDepFinder as Finder

main :: IO ()
main = do
  FinderOptions{..} <- getFinderOptions
  Finder.analyseDirectory hieDirectory cabalPlanFile
