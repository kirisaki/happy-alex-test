{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import Lexer
import Parser
import Control.Monad.State
import qualified Data.Map.Strict as MA
import GHC.IO.Unsafe

someFunc :: IO ()
someFunc = do
  --let s = "(\\x -> x + x) 1"
  --let s = "(\\x -> \\y -> x + y) 1 2"
  let s = "(\\x -> \\y -> \\z -> x y z) u v w"
  --let s = "(\\x -> x x)(\\x -> x x)"
  --let s = "(\\x -> \\x -> \\x -> x) 1"
  putStrLn s
  let ops = MA.fromList
            [ ("+", (6, OpL))
            , ("*", (7, OpL))
            , ("^", (8, OpR))
            ]
  let p = alexSetUserState (AlexUserState ops) >> parser
  case runAlex s p of
    Right tree -> do
      print $ reduce tree
      --print $ subst (Lambda "x" (Apply (Var "x") (Var "y"))) "x" (Lambda "x" (Var "x")) 0
      --print $ subst (Var "x") "y" (Var "y") 0
    Left e -> putStrLn e

subst :: Exp -> String -> Exp -> Int -> Exp
subst e2 x e1 cnt =
  case e1 of
    Var y -> if x == y then e2 else Var y
    Lambda y e ->
      Lambda y' $ subst e2 x (subst (Var y') y e (cnt + 1)) (cnt + 1)
      where
        y' = y <> "_" <> show cnt
    Apply e e' ->
      Apply (subst e2 x e $ cnt + 1) (subst e2 x e' $ cnt + 1)


step :: Exp -> [Exp]
step = \case
  Var x -> []
  Lambda x e0 -> fmap (Lambda x) (step e0)
  Apply e1 e2 ->
    (case e1 of
      Lambda x e0 -> [subst e2 x e0 0]
      _ -> []) ++
    (fmap (\e1' -> Apply e1' e2) (step e1)) ++
    (fmap (Apply e1) (step e2))

reduce :: Exp -> Exp
reduce e =
  case step e of
    [] -> e
    e':_ -> reduce e'
