module Lib
    ( someFunc
    ) where

import Lexer
import Control.Monad.State

someFunc :: IO ()
someFunc = do
  s <- getLine
  case runAlex s (Alex . const $ Left "nyaan") of
    Right tk -> undefined
    Left e -> putStrLn e
