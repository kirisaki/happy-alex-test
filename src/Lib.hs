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
      let env = MA.fromList
                [ ("succ", Val . Func $ \(Val (Num x)) -> Val . Num $ x + 1)
                , ("+", Val . Func $ \(Val (Num x)) -> Val . Func $ \(Val (Num y)) -> Val . Num $ x + y )
                ]
      print $ eval env tk
    Left e -> putStrLn e

type Environment = MA.Map String Exp

eval :: Environment -> Exp -> Either String Exp
eval _ v@(Val _) = pure  v
eval env (Var k) =
  case MA.lookup k env of
    Just exp -> pure exp
    _ -> Left $ "not found: " <> k
eval env (Apply (Apply (Lambda k0 l@(Lambda k1 exp)) arg0) arg1) = 
  let
    env' = MA.insert k0 arg0 env
  in
    eval env' $ Apply l arg1
eval env (Apply (Lambda k exp) arg) =
  let
    env' = MA.insert k arg env
  in
    eval env' exp
eval env (Apply (Val(Func f)) arg) =
  f <$> eval env arg
eval env (Apply exp@(Apply _ _) arg) = do
  exp' <- eval env exp
  eval env $ Apply exp' arg
eval env (Apply v@(Var _) arg) = do
  v' <- eval env v
  eval env $ Apply v' arg
eval env l@(Lambda _ _) = pure l
eval env exp = Left $ show env <> "  " <> show exp
