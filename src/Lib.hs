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
      print $ eval tk
    Left e -> putStrLn e

eval :: Exp -> Exp
eval v@(Num _) =  v
eval v@(Symbol _) = v
eval (Apply (Func f) x) = f (eval x)
eval (Apply (Symbol "+") ex) = Func $ \ey -> let Num y = eval ey in Num (x + y)
  where
    Num x = eval ex
eval (Apply (Symbol "*") ex) = Func $ \ey -> let Num y = eval ey in Num (x * y)
  where
    Num x = eval ex
eval (Apply (Symbol "^") ex) = Func $ \ey -> let Num y = eval ey in Num (x ^ y)
  where
    Num x = eval ex
    
eval (Apply e@(Apply _ _) x) = f x
  where
    Func f = eval e

