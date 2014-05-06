module Test(
              zero, one, two, three, four, succ, mul, true, false, and, or, neg, isZero,
              add, mult, par, prim, seg, phi, pred, y, r,
              testArithmetic, testLogic, testPairs, testRecurs, testRecursDefs) where 

import Lambda

zero    = "(Ls.Lz.z)"
one     = "(Ls.Lz.sz)"
two     = "(Ls.Lz.s(sz))"
three   = "(Ls.Lz.s(s(sz)))"
four    = "(Ls.Lz.s(s(s(sz))))"

prox    = "(Lw.Ly.Lx.y(wyx))"
mul     = "(Lx.Ly.Lz.x(yz))"
add     = "(Ln.Lm.(n" ++ prox ++ "m))"
mult n m = "(" ++ mul ++ n ++ m ++ ")"

true    = "(Lx.Ly.x)"
false   = "(Lx.Ly.y)"

land    = "(Lx.Ly.xy"++false++")"
lor     = "(Lx.Ly.x"++true++"y)"
neg     = "(Lx.x"++false++true++")"
isZero  = "(Lx.x"++false++neg++false++")"

par  a b = "(Lz.z"++a++b++")"
prim par = "("++par++true++")"
seg  par = "("++par++false++")"

phi   = "(Lp.Lz.z("++prox++(prim "p")++")("++(prim "p")++"))"
prev  = "(Ln.n"++phi++(par zero zero)++false++")"
y     = "(Ly.(Lx.y(xx))(Lx.y(xx)))"
r     = "(Lr.Ln."++isZero++"n"++zero++"(n"++prox++"(r("++prev++"n))))"

testArithmetic =
    do
      putStrLn ("0 = " ++ zero)
      putStrLn ("1 = " ++ one)
      putStrLn ("2 = " ++ two)
      putStrLn ("3 = " ++ three)
      putStrLn ("4 = " ++ four)
      putStrLn ("SUCC = " ++ prox)
      putStrLn "-----------------------"
      putStrLn ("SUCC 3 = " ++ prox ++ three)
      evalStr (prox ++ three)
      putStrLn "-----------------------"
      putStrLn ("ADD = " ++ add)
      putStrLn ("ADD 2 3 = " ++ add ++ two ++ three)
      evalStr (add ++ two ++ three)
      putStrLn "-----------------------"
      putStrLn ("MULT  = " ++ mul)
      putStrLn ("MULT 3 2 = " ++ (mult three two)) 
      evalStr (mult three two)
      putStrLn "-----------------------"

testLogic =
    do
      putStrLn ("TRUE  = " ++ true)
      putStrLn ("FALSE = " ++ false)
      putStrLn ("NOT   = " ++ neg)
      putStrLn "-----------------------"
      putStrLn ("NOT TRUE = " ++ neg ++ true)
      evalStr (neg++true)
      putStrLn "-----------------------"
      putStrLn ("NOT FALSE = " ++ neg ++ false)
      evalStr (neg++false)
      putStrLn "-----------------------"
      putStrLn ("AND = " ++ land)
      putStrLn ("OR  = " ++ lor)
      putStrLn "-----------------------"
      putStrLn ("AND TRUE TRUE = " ++ land ++ true ++ true)
      evalStr (land ++ true ++ true)
      putStrLn "-----------------------"
      putStrLn ("AND TRUE FALSE = " ++ land ++ true ++ false)
      evalStr (land ++ true ++ false)
      putStrLn "-----------------------"
      putStrLn ("AND FALSE TRUE = " ++ land ++ false ++ true)
      evalStr (land ++ false ++ true)
      putStrLn "-----------------------"
      putStrLn ("AND FALSE FALSE = " ++ land ++ false ++ false)
      evalStr (land ++ false ++ false)
      putStrLn "-----------------------"
      putStrLn ("OR TRUE TRUE = " ++ lor ++ true ++ true)
      evalStr (lor ++ true ++ true)
      putStrLn "-----------------------"
      putStrLn ("OR TRUE FALSE = " ++ lor ++ true ++ false)
      evalStr (lor ++ true ++ false)
      putStrLn "-----------------------"
      putStrLn ("OR FALSE TRUE = " ++ lor ++ false ++ true)
      evalStr (lor ++ false ++ true)
      putStrLn "-----------------------"
      putStrLn ("OR FALSE FALSE = " ++ lor ++ false ++ false)
      evalStr (lor ++ false ++ false)
      putStrLn "-----------------------"
      putStrLn ("0? = " ++ isZero)
      putStrLn ("0? 0 = " ++ isZero ++ zero)
      evalStr (isZero ++ zero)
      putStrLn "-----------------------"
      putStrLn ("0? 3 = " ++ isZero ++ three)
      evalStr (isZero ++ three)
      putStrLn "-----------------------"

testPairs =
    do
      putStrLn ("(a,b) = " ++ (par "a" "b"))
      putStrLn ("FIRST (a,b) = (a,b)TRUE = " ++ (par "a" "b") ++ true)
      evalStr ((par "a" "b") ++ true)
      putStrLn "-----------------------"
      putStrLn ("SECOND (a,b) = (a,b)FALSE = " ++ (par "a" "b") ++ false)
      evalStr ((par "a" "b") ++ false)
      putStrLn "-----------------------"
      putStrLn ("PHI = " ++ phi)
      putStrLn  "PHI (n,k) = (n+1,n)"
      putStrLn ("  => PHI (3,1) = (4,3)")
      putStrLn ("(3,1) = " ++ (par three one))
      putStrLn ("(4,3) = " ++ (par four three))
      putStrLn ("PHI (3,1) = " ++ phi ++ (par three one))
      evalStr (phi ++ (par three one))
      putStrLn "-----------------------"
      putStrLn "PREV = (Ln.n PHI (0,0) FALSE)"
      putStrLn ("PREV = " ++ prev)
      putStrLn ("PREV 3 = " ++ prev ++ three)
      evalStr (prev ++ three)
      putStrLn "-----------------------"

testRecursDefs =
    do
      putStrLn ("Y = " ++ y)
      putStrLn ("SUM n = if (?0 n) 0 else n + sum(n-1)")
      putStrLn (" implemented with Y combinator")
      putStrLn  "R = (Lrn.0?n0(ADD n (r(PREV n))))"
      putStrLn ("R = " ++ r)
      putStrLn "SUM n = YR n"
      putStrLn ("SUM 3 = " ++ y ++ r ++ three) 
      putStrLn "-----------------------"

testRecurs =
    do
      putStrLn ("SUM 3 = " ++ y ++ r ++ three) 
      evalStr (y ++ r ++ three)
      putStrLn "-----------------------"

