module Lib
    ( someFunc
    ) where

import Lexer
import Parser
import Control.Monad.State
import qualified Data.Map.Strict as MA

someFunc :: IO ()
someFunc = do
  s <- getLine
  let ops = MA.fromList
            [ ("+", (6, OpL))
            , ("*", (7, OpL))
            , ("^", (8, OpR))
            ]
  let p = alexSetUserState (AlexUserState ops) >> parser
  case runAlex s p of
    Right tk -> do
      print tk
      print $ runStateT (eval tk) MA.empty
    Left e -> putStrLn e

eval :: Exp -> StateT (MA.Map String Exp) (Either String) Exp
eval v@(Num _) =  pure v
eval (Var k) = do
  table <- get
  case MA.lookup k table of
    Just v -> pure v
    _ -> fail k
