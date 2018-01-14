module Main where

import System.Environment

import Lib

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--start-db"] -> do
      startDb
      waitForDbToBeReady
    _              -> return ()

  setupDb
  generateContent
