module Lambda(
              Expression,
              compileExpression,
              substitute,
              member,
              addVar, addVars,
              eval, loop,
              getVars, boundVars, freeVars, replVars,
              iguales,
              display,
              evalStr
             ) where

import Utility

data Expression = Var Char | Lambda Char Expression | Apply Expression Expression |
                  Error [Char] [Char] deriving (Show, Eq)

-- Syntax
-- Expression -> Expression Term | Term
-- Term -> L<name>.Expression | ( Expression )

-- compila la expression en una estructura (AST) en haskell
-- la siguiente expresion compila 2+3 con la espresion
-- 2S3 donde S es la funcion sucesor y 2 y 3 son los numeros
-- expresados en calculo lambda
--
-- compile "(Ls.Lz.s(sz))(Lw.Ly.Lx.y(wyx))(Lu.Lv.u(u(uv)))"

compile :: [Char] -> Expression
compile input = 
    expression
    where (expression, _) = compileExpression input

compileExpression :: [Char] -> (Expression, [Char])
compileExpression [] = (Error "end of input" [], [])
compileExpression input =
    let
        (term, rest) = compileTerm input
    in
      compileExpressionRest term rest

compileExpressionRest :: Expression -> [Char] -> (Expression, [Char])

compileExpressionRest term [] = (term, [])
compileExpressionRest term input =
    let
        (expression, restOfInput) = compileTerm input
    in
      case expression of
        (Error _ _) -> (term, input)
        term2       -> compileExpressionRest (Apply term term2) restOfInput

compileTerm :: [Char] -> (Expression, [Char])
compileTerm []         = (Error "end of input" [], [])
compileTerm ('(':input) =
    let
        (expr, rest) = compileExpression input
    in
      case rest of
        (')':_) -> (expr, (tail rest))
        _          -> (Error "expected )" rest, rest)

compileTerm ('L':x:'.':input) =
    if not (isAlpha x)
    then (Error ("variable expected, got "++[x]++"instead") (x:'.':input), (x:'.':input))
    else
        let
            (expr, rest) = compileExpression input
        in
          (Lambda x expr, rest)

compileTerm (x:input) = 
    if isAlpha x
    then (Var x, input)
    else (Error ("invalid symbol " ++ [x]) (x:input), (x:input))

substitute :: Char -> Expression -> Expression -> Expression -> (Expression, Expression)
substitute x (Var y) subst upSubst=
    if x == y
    then (subst, upSubst)
    else (Var y, Var y)

substitute x (Lambda y expr) subst upSubst =
    if x == y
    then (Lambda y expr, Lambda y expr)
    else (Lambda y substFun, Lambda y upFun)
        where 
          (substFun, upFun) = (substitute x expr subst upSubst)

substitute x (Apply exp1 exp2) subst upSubst =
    (Apply subsExp1 subsExp2, Apply upExp1 upExp2)
    where
      (subsExp1, upExp1) = substitute x exp1 subst upSubst
      (subsExp2, upExp2) = substitute x exp2 subst upSubst

getVars :: Expression -> [Char]
getVars (Var x) = [x]
getVars (Apply e1 e2)  = addVars (getVars e1) (getVars e2)
getVars (Lambda x fun) = addVar x (getVars fun)

boundVars :: Expression -> [Char]
boundVars (Var x) = []
boundVars (Apply e1 e2)  = addVars (boundVars e1) (boundVars e2)
boundVars (Lambda x fun) = addVar x (boundVars fun)

freeVars :: Expression -> [Char]
freeVars e = setSub (getVars e) (boundVars e)

upExpr :: Expression -> Expression
upExpr (Var x)        = (Var (toUpper x))
upExpr (Apply e1 e2)  = (Apply (upExpr e1) (upExpr e2)) 
upExpr (Lambda x fun) = (Lambda (toUpper x) (upExpr fun))

upVar :: Char -> Expression -> Expression
upVar x (Var y) =
    if x == y
    then (Var (toUpper x))
    else (Var y)
upVar x (Lambda y expr) =
    if x == y
    then (Lambda (toUpper y) (upVar x expr))
    else (Lambda y (upVar x expr))
upVar x (Apply exp1 exp2) =
    (Apply (upVar x exp1) (upVar x exp2))

replVars :: Expression -> [(Char,Char)] -> [Char] -> Expression
replVars (Var x) map bound = 
    if (member x bound)
    then (Var (subst x map))
    else (Var x)
replVars (Apply e1 e2) map bound  = (Apply (replVars e1 map bound) (replVars e2 map bound))
replVars (Lambda x fun) map bound = (Lambda (subst x map) (replVars fun map (x:bound)))

-- iguales revisa que dos expresiones sean iguales
-- Lx.x debe ser igual a Ly.y, por ejemplo
iguales :: Expression -> Expression -> Bool
iguales exp1 exp2 = 
    result
    where (result, _) = equal exp1 exp2 []

equal :: Expression -> Expression -> [(Char,Char)] -> (Bool, [(Char, Char)])
equal (Var a) (Var b) binding = bind a b binding

equal (Apply exp1 exp2) (Apply exp3 exp4) binding =
    let
        (result, newBinding) = equal exp1 exp3 binding
    in
      if not result
      then (False, binding)
      else equal exp2 exp4 newBinding

equal (Lambda n exp1) (Lambda m exp2) binding =
    let
        (result, newBinding) = bind n m binding
    in
      if not result
      then (False, binding)
      else equal exp1 exp2 newBinding
equal _ _ binding = (False, binding)

eval :: Expression -> [Char] -> (Expression, Expression, Expression)
eval (Var x) _ = (Var x, Var x, Var x)
eval (Apply (Lambda x fun) exp) free = 
    let
        varsExp      = getVars exp
        varsFun      = getVars fun
        boundvarsFun = boundVars fun
        boundvarsExp = boundVars exp
        overlapFun   = (inter varsExp boundvarsFun) 
        overlapExp   = (inter varsFun boundvarsExp)
        newVarsFun   = take (length overlapFun) [x | x <- ['a'..], not (member x (free ++ varsFun ++ varsExp))]
        newVarsExp   = take (length overlapExp) [x | x <- ['a'..], not (member x (free ++ varsFun ++ varsExp ++ newVarsFun))]
        cleanFun     = replVars fun (zip overlapFun newVarsFun) []
        cleanExp     = replVars exp (zip overlapExp newVarsExp) []
        upExp        = (upExpr cleanExp)
        (subst, upSubst) = substitute x cleanFun cleanExp upExp
    in
    (subst, Apply (Lambda (toUpper x) (upVar x cleanFun)) upExp, upSubst)

eval (Apply e1 e2) free =
    let (e, esubst, upper) = eval e1 free
    in
      if not (iguales e e1)
      then (Apply e e2, Apply esubst e2, Apply upper e2)
      else (Apply e e2new, Apply e e2subst, Apply e e2upper)
           where (e2new, e2subst, e2upper) = eval e2 free

eval (Lambda x fun) free = 
    (Lambda x expr, Lambda x subst, Lambda x upper)
    where (expr, subst, upper) = eval fun (x:free)

loop expression = 
    let
      (result, subst, upper) = eval expression (freeVars expression)
    in
      if (iguales expression result)
      then do putStrLn (display expression)
      else do
        putStrLn (display expression)
        putStrLn (display subst)
        putStrLn (display upper)
        putStrLn "-----"
        loop result

evalStr input = loop (compile input)

display :: Expression -> [Char]
display (Var x)  = [x]
display (Lambda x e)  = "(L" ++ [x]++"."++(display e) ++ ")"
display (Apply e1 e2) = 
    case e2 of
      (Apply _ _) -> (display e1) ++ "(" ++ (display e2) ++ ")"
      _           -> (display e1) ++ (display e2)

