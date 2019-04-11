-- AlexLexerExample.x
{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
  
module Lexer where

import Control.Monad.State
import qualified Data.Map.Strict as MA
}

%wrapper "monadUserState"

$digit = 0-9
$smallalpha = a-z
$largealpha = A-Z
$alpha = [$smallalpha$largealpha]
$symbol = [\+\-\*\/]

tokens :-

  $white+                       { mkLx LxWhite }
  $digit+                       { mkLx LxNum }
  \;                            { mkLx LxSep }
  \(                            { mkLx LxLParen }
  \)                            { mkLx LxRParen }
  [\=\+\-\*\/\^]                  { mkLx LxVarSym }

{
data Lexeme
  = LxWhite
  | LxSep
  | LxLParen
  | LxRParen
  | LxVarSym
  | LxNum
  deriving (Eq, Show)

data Token
  = TkWhite AlexPosn
  | TkSep AlexPosn
  | TkLParen AlexPosn
  | TkRParen AlexPosn
  | TkVarSym ((String, Int, OpAssoc), AlexPosn)
  | TkNum (Integer, AlexPosn)
  | TkEof
  deriving (Eq, Show)

mkLx :: Lexeme -> AlexInput -> Int -> Alex Token
mkLx lx (pos, _, _, str) len =
  let
    t = take len str
  in
    case lx of
      LxWhite -> pure $ TkWhite pos
      LxSep -> pure $ TkSep pos
      LxLParen -> pure $ TkLParen pos
      LxRParen -> pure $ TkRParen pos
      LxVarSym -> Alex $ (\s@AlexState{..} ->
                          case MA.lookup t (operators alex_ust) of
                            Just (n, asoc) -> Right (s, TkVarSym ((t, fromIntegral n, asoc), pos))
                            _ -> Left "unknown operator"
                         )
      LxNum -> pure $ TkNum (read t,  pos)

alexEOF :: Alex Token
alexEOF = pure TkEof

data OpAssoc
  = OpL
  | OpR
  deriving(Show,Eq)

data AlexUserState = AlexUserState { operators :: MA.Map String (Int, OpAssoc) }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState MA.empty

}
