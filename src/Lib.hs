{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import Lexer
import Parser
import Control.Monad.State
import qualified Data.Map.Strict as MA
import qualified Data.Set as SE
import qualified Data.List as L
import GHC.IO.Unsafe

type Context = MA.Map String Type

type Constraint = SE.Set (Type, Type)

type Assign = (String, Type)


constraintType :: Context -> Exp -> [String] -> Either String (Type, Constraint, [String])
constraintType _ (Lit (LitInt _)) names = pure (TyInt, SE.empty, names)
constraintType ctx (Var x) names = do
  typ <- case MA.lookup x ctx of
    Just typ' -> pure typ'
    Nothing -> Left $ "not found type variable: " <> show x
  pure (typ, SE.empty, names)
constraintType ctx (Lambda x t1) names = do
  (xt, names0) <- newTyVar names
  let ctx0 = MA.insert x xt ctx
  (typ1, cst1, names1) <- constraintType ctx0 t1 names0
  pure (TyFun xt typ1, cst1, names1)
constraintType ctx (Apply t1 t2) names = do
  (typ1, cst1, names1) <- constraintType ctx t1 names
  (typ2, cst2, names2) <- constraintType ctx t2 names1
  (typ, names3) <- newTyVar names2
  let cst = SE.unions [cst1, cst2, SE.singleton (typ1, TyFun typ2 typ)]
  pure (typ, cst, names3)

newTyVar :: [String] -> Either String (Type, [String])
newTyVar (n:ns) = pure (TyVar n, ns)
newTyVar []   = Left "no unused type variables"

unify :: Constraint -> Either String [Assign]
unify c
  | SE.null c = pure []
  | otherwise =
    let
      eq = SE.elemAt 0 c
      c' = SE.deleteAt 0 c
    in
      case eq of
        (s, t) | s == t -> unify c'
        (TyVar x, t) | SE.notMember x (freeVars t) -> do
                         let a = (x, t)
                         as <- unify (assignConstraint [a] c')
                         pure (a:as)
        (s, TyVar x) | SE.notMember x (freeVars s) -> do
                                let a = (x, s)
                                as <- unify (assignConstraint [a] c')
                                pure (a:as)
        (TyFun s1 s2, TyFun t1 t2) ->
          unify (SE.insert (s1, t1) (SE.insert (s2, t2) c'))
        _ -> Left "invalid constraints"

freeVars :: Type -> SE.Set String
freeVars (TyVar x) = SE.singleton x
freeVars TyInt = SE.empty
freeVars (TyFun t1 t2) = SE.union (freeVars t1) (freeVars t2)

assignType :: [Assign] -> Type -> Type
assignType as t =
  L.foldl' (flip assignType') t as

assignType' :: Assign -> Type -> Type
assignType' (y, s) t'@(TyVar x)
  | x == y = s
  | otherwise = t'
assignType' _ TyInt = TyInt
assignType' a (TyFun t1 t2) = TyFun (assignType' a t1) (assignType' a t2)

assignConstraint :: [Assign] -> Constraint -> Constraint
assignConstraint as cst =
  L.foldl' go cst as
  where
    go cst' a = SE.map (\(t1, t2) -> (assignType' a t1, assignType' a t2)) cst'

someFunc :: IO ()
someFunc = do
  --let s = "(\\x -> x + x) 1"
  --let s = "(\\x -> \\y -> x + y) u v"
  --let s = "(\\x -> \\y -> \\z -> x y z) u v w"
  --let s = "(\\x -> x x)(\\x -> x x)"
  --let s = "(\\x -> \\x -> \\x -> x) 1"
  let s = "(\\x -> \\y -> \\z -> x + y) 1 2 3"
  putStrLn s
  let ops = MA.fromList
            [ ("+", (6, OpL))
            , ("*", (7, OpL))
            , ("^", (8, OpR))
            ]
  let p = alexSetUserState (AlexUserState ops) >> parser
  let names = show <$> [1 :: Int ..]
  case runAlex s p of
    Right tree -> do
      print tree
      let Right (t, c, ns) = constraintType (MA.fromList [("+", TyFun TyInt (TyFun TyInt TyInt))]) tree names
      print t >> print c >> print (head ns)
      let Right a = unify c
      print a
      print $ assignType a t
      --print $ subst (Lambda "x" (Apply (Var "x") (Var "y"))) "x" (Lambda "x" (Var "x")) 0
      --print $ subst (Var "x") "y" (Var "y") 0
    Left e -> putStrLn e

subst :: Exp -> String -> Exp -> Int -> Exp
subst e2 x e1 cnt =
  case e1 of
    Var y -> if x == y then e2 else Var y
    Lit n -> Lit n
    Lambda y e ->
      Lambda y' $ subst e2 x (subst (Var y') y e (cnt + 1)) (cnt + 1)
      where
        y' = y <> "_" <> show cnt
    Apply (Lit _) _ -> error "can't apply to value"
    Apply e e' ->
      Apply (subst e2 x e $ cnt + 1) (subst e2 x e' $ cnt + 1)


step :: Exp -> [Exp]
step = \case
  Var _ -> []
  Lit _ -> []
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
