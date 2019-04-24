
{
module Parser where
import Lexer
import qualified Data.Map as Map
}

%name parser
%error { parseError }
%lexer { lexwrap } { TkEof }
%monad { Alex }
%tokentype { Token }

%token
' '     { TkWhite _ }
NUM     { TkNum ($$, _) }
VARSYM  { TkVarSym ($$, _) }
VARID   { TkVarId ($$, _) }
';'     { TkSep _ }
'('     { TkLParen _ }
')'     { TkRParen _ }
'\\'    { TkLambda _ }
'->'    { TkArrow _ }

%left APPLY

%%
program :: { Exp }
program:        exp        { $1 }

exp :: { Exp }
exp
  : exp term %prec APPLY { Apply $1 $2 }
  | term { $1 }
  | exp VARSYM exp            { binop $2 $1 $3 }
  | exp VARSYM exp VARSYM exp { assoc $1 $2 $3 $4 $5 }

term :: { Exp }
term
  : lambda                    { $1 }
  | varid                      { $1 }
--  | num                       { $1 }
  | '(' exp ')'               { $2 }

varid :: { Exp }
varid
  : VARID { Var $1 }

--num :: { Exp }
--num
--  : NUM { Val (Num $1) }

lambda :: { Exp }
lambda
  : '\\' VARID '->' exp { Lambda $2 $4 }


{

mkVarSym :: (String, Int, OpAssoc) -> Exp
mkVarSym (k, _, _) = Var k

data Exp
  = Apply Exp Exp
  | Lambda String Exp
  | Var String

data Val
  = Num Integer
  | Func (Exp -> Exp)

instance Show Exp where
  show (Apply x y) = "Apply(" <> show x <> ")(" <> show y <> ")"
  show (Lambda x exp) = "(Lambda \"" <> x <> "\" " <> show exp <> ")"
  show (Var x) = "Var \"" <> x <> "\""

instance Show Val where
  show (Num v) = "Num " <> show v
  show (Func _) = "Func"
  
binop :: (String, Int, OpAssoc) -> Exp -> Exp -> Exp
binop (op, _, _) lhs rhs = Apply (Apply (Var op) lhs) rhs

assoc :: Exp -> (String, Int, OpAssoc) -> Exp -> (String, Int, OpAssoc) -> Exp -> Exp
assoc ex1 op1@(sy1, pr1, as1) ex2 op2@(sy2, pr2, as2) ex3
  | pr1 > pr2 = binop op2 (binop op1 ex1 ex2) ex3
  | pr1 < pr2 = binop op1 ex1 (binop op2 ex2 ex3)
  | as1 == OpL && as2 == OpL = binop op2 (binop op1 ex1 ex2) ex3
  | as1 == OpR && as2 == OpR = binop op1 ex1 (binop op2 ex2 ex3)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t


}
