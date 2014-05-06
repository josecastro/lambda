module Utility(isAlpha,toUpper,member,addVar,addVars,setSub,inter,subst,bind) where

import Data.Char (ord,chr)

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a') && (c <= 'z')

toUpper :: Char -> Char
toUpper c = chr(ord(c)+ord('A')-ord('a'))

member :: Char -> [Char] -> Bool
member _ [] = False
member x (c:rest) = 
    if x == c
    then True
    else (member x rest)

addVar :: Char -> [Char] -> [Char]
addVar x [] = [x]
addVar x vars =
    if (member x vars)
    then vars
    else x:vars

addVars :: [Char] -> [Char] -> [Char]
addVars [] vars = vars
addVars (x:rest) vars = addVars rest (addVar x vars)

setSub :: [Char] -> [Char] -> [Char]
setSub a b = [x | x <- a, not (member x b)]

inter :: [Char] -> [Char] -> [Char]
inter a b = [x | x <- a, (member x b)]

subst :: Char -> [(Char,Char)] -> Char
subst c [] = c
subst c ((a,b):rest) =
    if c == a
    then b
    else subst c rest

-- bind es parecido a assoc
-- asocia dos variables a y b
-- si alguna ya tiene valor y este valor es distinto
-- a la otra variable, entonces devuelve falso

bind :: Char -> Char -> [(Char,Char)] -> (Bool, [(Char,Char)])
bind a b [] = (True, [(a,b),(b,a)])
bind a b ((c,d):rest) =
    case c of
      a -> if d == b
           then (True, ((c,d):rest))
           else (value, ((c,d):result))
      b -> if d == a
           then (True, ((c,d):rest))
           else (value, ((c,d):result))
      _ -> if (d == a) || (d == b)
           then (False, ((c,d):rest))
           else (value, ((c,d):result))
    where (value, result) = bind a b rest

