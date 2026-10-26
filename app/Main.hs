{-# LANGUAGE OverloadedStrings #-}

module Main where

import LGPT.Helpers
import LGPT.TUI qualified as TUI
import Control.Monad.State
import qualified Data.Map as Map
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment

{- | This is what gets run when you run the program. 

    It just calls the runREPL function in TUI.hs, which is where the real work 
    happens :)
-}
main :: IO ()
main = do
  -- Pre-initialisation to set up the terminal
  runStart
  -- Actually run the loop!
  runStateT TUI.runREPL Map.empty >> pure ()  -- runStateT executes the computation of the REPL, with an empty map passed as the initial state.
