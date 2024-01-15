module UnitTests where

import Main
import Unify
import Interpret

----------------------------------------------------------------------------------------------------------
-- TEST MAIN ---------------------------------------------------------------------------------------------

-- Test interpretFile

test :: IO [Unifier]
test = do
    db <- interpretFile "Tests/test.pl"
    let
        -- node = Node [Pterm "proud" [JustPvar (pVar "Z")]] []
        -- node = Node [Pterm "sum" [Pterm "zero" [], Pterm "zero" [], JustPvar (pVar "X")]] []
        node = Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "zero" []]], Pterm "s" [Pterm "s" [Pterm "zero" []]], JustPvar (pVar "X")]] []
        -- node = Node [Pterm "ancestor" [Pterm "gosho" [], JustPvar (pVar "Y")]] []
        res = resolve node db
        in return res

test2 :: IO [Unifier]
test2 = do
    db <- interpretFile "Tests/test.pl"
    let
        -- node = Node [Pterm "proud" [JustPvar (pVar "Z")]] []
        -- node = Node [Pterm "sum" [Pterm "zero" [], Pterm "zero" [], JustPvar (pVar "X")]] []
        node = Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "zero" []]], Pterm "s" [Pterm "s" [Pterm "zero" []]], JustPvar (pVar "X")]] []
        -- node = Node [Pterm "ancestor" [Pterm "gosho" [], JustPvar (pVar "Y")]] []
        res = resolve node db
        in return res

-- Test compile

test11 = compile "a(b) :- c(d)."

test12 = compile "?- a(b)."

test13 = compile "a(b(c, d)) :- X."
-- грешка - лявата страна на правилото не е атом

test14 = compile "a(b, c)"
-- грешка - редът не завършва с точка


----------------------------------------------------------------------------------------------------------
-- UNIFY -------------------------------------------------------------------------------------------------

test21_term = Pterm "a" []
test21_resolvent = Pterm "b" []
test21 = plUnify test21_term test21_resolvent
-- Output: Nothing (cannot be resolved, false)

test_term = Pterm "exists" [JustPvar (pVar "F")]
test_resolvent = Pterm "exists" [Pterm "a" []]
test22 = plUnify test_term test_resolvent
-- Output: Just [F = a]

test23 = plUnifierApplyToUnifier [PLEquation (pVar "Y") (Pterm "a" [])] [PLEquation (pVar "X") (Pterm "b" [JustPvar (pVar "Y")])]
-- Ouput: [PLEquation X = b(a)]

test24 = plUnifierApplyToTerm [PLEquation (pVar "X") (Pterm "a" []), PLEquation (pVar "Y") (Pterm "b" []), PLEquation (pVar "Z") (Pterm "c" [])]
    (Pterm "term" [JustPvar (pVar "X"), JustPvar (pVar "X"), JustPvar (pVar "Z")])
-- Output: Pterm "term" [Pterm "a" [], Pterm "a" [], JustPvar (Pvar "Z")]

test26 = plUnify (Pterm "proud" [JustPvar (pVar "Z")]) (Pterm "proud" [JustPvar (pVar "X")])
-- Just [Z = X]

test27 = plUnify (Pterm "parent" [Pterm "charles" [], Pterm "elizabeth" []])
    (Pterm "parent" [JustPvar (pVar "X"), JustPvar (pVar "X")])
-- Nothing
-- % No match, X cannot have two different values.

test28 = plUnify (Pterm "parent" [Pterm "charles" [], JustPvar (pVar "X")])
    (Pterm "parent" [JustPvar (pVar "Y"), JustPvar (pVar "Y")])

test29 = plUnify (Pterm "p" [JustPvar (pVar "X"), JustPvar (pVar "X")]) (Pterm "p" [Pterm "f" [Pterm "a" []], Pterm "f" [Pterm "b" []]])
-- Nothing

test30 = plUnify (Pterm "p" [JustPvar (pVar "X"), JustPvar (pVar "Y")]) (Pterm "p" [Pterm "f" [Pterm "a" []], Pterm "f" [Pterm "b" []]])
-- [Y = f(b), X = f(a)]

test31 = plUnify (Pterm "p" [JustPvar (pVar "X"), Pterm "s" [JustPvar (pVar "Y")]]) (Pterm "p" [JustPvar (pVar "Z"), JustPvar (pVar "Z")])
-- p(X,s(Y)) p(Z,Z).
-- [Z = s(Y), X = s(Y)]

----------------------------------------------------------------------------------------------------------
-- INTERPRET ---------------------------------------------------------------------------------------------


test32 = genn (Node [Pterm "proud" [JustPvar (pVar "Z")]] [])
    [Prule (Pterm "proud" [JustPvar (pVar "X")])
        [Pterm "parent" [JustPvar (pVar "X"), JustPvar (pVar "Y")],
         Pterm "newborn" [JustPvar (pVar "Y")]],
     Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
     Pfact (Pterm "newborn" [Pterm "ann" []])]

-- tautology
test37 = genn (Node [Pterm "newborn" [Pterm "ann" []]] [])
    [Pfact (Pterm "newborn" [Pterm "ann" []])]
-- Just [Node [] [Just []]]

test38 = do
    db <- interpretFile "Tests/test.pl"
    let
        --node = Node [Pterm "child" [JustPvar (Pvar "X"), Pterm "john" []]] []
        node = Node [Pterm "natNumber" [JustPvar (pVar "X")]] []
        res = resolve node db
        in return res

--

test39 = do
    db <- interpretFile "Tests/test.pl"
    let
        --node = Node [Pterm "child" [JustPvar (Pvar "X"), Pterm "john" []]] []
        node = Node [Pterm "even" [JustPvar (pVar "X")]] []
        res = resolve node db
        in return res


test50 = plUnify (Pterm "natNumber" [JustPvar (pVar "X")])  (Pterm "natNumber" [Pterm "zero" []])
-- {X = zero}
test51 = plUnify (Pterm "natNumber" [JustPvar (pVar "X")])  (Pterm "natNumber" [Pterm "succ" [JustPvar (pVar "X")]])
-- Nothing

test52 = plUnifierApplyToUnifier [PLEquation (pVar "X") (Pterm "zero" [])] [PLEquation (pVar "X") (Pterm "succ" [JustPvar (pVar "X")])]


test53  = plUnify (Pterm "sum" [Pterm "zero" [], Pterm "zero" [], JustPvar (pVar "X")])
    (Pterm "sum" [JustPvar (pVar "N"), Pterm "zero" [], JustPvar (pVar "N")])
-- sum(N, zero, N).
-- test33 = 
--     -- case test32 of
--     genn (head test32)
--     [Prule (Pterm "proud" [JustPvar (Pvar "X")])
--         [Pterm "parent" [JustPvar (Pvar "X"), JustPvar (Pvar "Y")],
--          Pterm "newborn" [JustPvar (Pvar "Y")]],
--      Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
--      Pfact (Pterm "newborn" [Pterm "ann" []])]
-- test34 = genn (head test33)
--     [Prule (Pterm "proud" [JustPvar (Pvar "X")])
--         [Pterm "parent" [JustPvar (Pvar "X"), JustPvar (Pvar "Y")],
--          Pterm "newborn" [JustPvar (Pvar "Y")]],
--      Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
--      Pfact (Pterm "newborn" [Pterm "ann" []])]
-- 
-- test35 = genn (head test34)
--     [Prule (Pterm "proud" [JustPvar (Pvar "X")])
--         [Pterm "parent" [JustPvar (Pvar "X"), JustPvar (Pvar "Y")],
--          Pterm "newborn" [JustPvar (Pvar "Y")]],
--      Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
--      Pfact (Pterm "newborn" [Pterm "ann" []])]
-- 
-- test36 = genn (head test35)
--     [Prule (Pterm "proud" [JustPvar (Pvar "X")])
--         [Pterm "parent" [JustPvar (Pvar "X"), JustPvar (Pvar "Y")],
--          Pterm "newborn" [JustPvar (Pvar "Y")]],
--      Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
--      Pfact (Pterm "newborn" [Pterm "ann" []])]
-- 
-- 
-- --
-- test37 = genn (Node [Pterm "proud" [JustPvar (Pvar "Z")]] [])
--     [Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
--      Pfact (Pterm "newborn" [Pterm "ann" []])]

-- TEST PROGRAM 2
prog2 =
    [Pfact (Pterm "parent" [Pterm "pesho" [], Pterm "gosho" []]),
     Pfact (Pterm "parent" [Pterm "gosho" [], Pterm "ivan" []]),
     Pfact (Pterm "parent" [Pterm "ivan" [], Pterm "penka" []]),
     Pfact (Pterm "parent" [Pterm "penka" [], Pterm "asen" []]),
     Pfact (Pterm "ancestor" [JustPvar (pVar "X"), JustPvar (pVar "X")]),
     Prule (Pterm "ancestor" [JustPvar (pVar "X"), JustPvar (pVar "Z")])
           [Pterm "parent" [JustPvar (pVar "X"), JustPvar (pVar "Y")], Pterm "ancestor" [JustPvar (pVar "Y"), JustPvar (pVar "Z")]]]

test60 = plUnify (Pterm "ancestor" [JustPvar (pVar "X"), JustPvar (pVar "Z")]) (Pterm "ancestor" [Pterm "gosh" [], JustPvar (pVar "Y")])

test61 = resolve (Node [Pterm "ancestor" [Pterm "gosho" [], JustPvar (pVar "Y")]] []) prog2
test62 = genn (Node [Pterm "parent" [Pterm "gosho" [], JustPvar (pVar "Y")],
               Pterm "ancestor" [JustPvar (pVar "Y"), JustPvar (pVar "Z")]]
               [Just [PLEquation (pVar "Y") (JustPvar (pVar "Z")), PLEquation (pVar "X") (Pterm "gosho" [])]]) prog2
test63 = genn (Node [Pterm "parent" [Pterm "gosho" [], Pterm "ivan" []],
               Pterm "ancestor" [JustPvar (pVar "ivan"), JustPvar (pVar "Z")]]
               [Just [PLEquation (pVar "Y") (Pterm "ivan" [])], Just [PLEquation (pVar "Y") (JustPvar (pVar "Z")), PLEquation (pVar "X") (Pterm "gosho" [])]]) prog2

-- TEST PROGRAM 3
prog3 = 
    [Pfact (Pterm "sum" [JustPvar (pVar "N"), Pterm "z" [], JustPvar (pVar "N")]),
     Prule (Pterm "sum" [JustPvar (pVar "N"), Pterm "s" [JustPvar (pVar "M")], Pterm "s" [JustPvar (pVar "K")]])
           [Pterm "sum" [JustPvar (pVar "N"), JustPvar (pVar "M"), JustPvar (pVar "K")]]]

test70 = resolve (Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "z" []]], Pterm "s" [Pterm "s" [Pterm "z" []]], JustPvar (pVar "X")]] [])
    prog3

test71 = genn (Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "z" []]], Pterm "s" [Pterm "s" [Pterm "z" []]], JustPvar (pVar "X")]] [])
    prog3

test72 = genn (Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "z" []]], Pterm "s" [Pterm "z" []], JustPvar (pVar "K")]] [])
    prog3

test73 = genn (Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "z" []]], Pterm "z" [], JustPvar (pVar "K")]] [])
    prog3

test74 = plUnify (Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "z" []]], Pterm "z" [], JustPvar (pVar "K")])
    (Pterm "sum" [JustPvar (pVar "N"), Pterm "z" [], JustPvar (pVar "N")])

-- append(empty, L, L).
-- append(cons(H, T1), L2, cons(H, T3)) :- append(T1, L2, T3).
-- 
-- ?- append(cons(baba, cons(dyado, empty)), cons(lelya, cons(chicho, empty)), L)
prog4 = 
    [Pfact (Pterm "append" [Pterm "empty" [], JustPvar (pVar "L"), JustPvar (pVar "L")]),
     Prule (Pterm "append" [Pterm "cons" [JustPvar (pVar "H"), JustPvar (pVar "T1")], JustPvar (pVar "L2"), Pterm "cons" [JustPvar (pVar "H"), JustPvar (pVar "T3")]])
        [Pterm "append" [JustPvar (pVar "T1"), JustPvar (pVar "L2"), JustPvar (pVar "T3")]]]

test80 = resolve (Node [Pterm "append"
    [Pterm "cons" [Pterm "baba" [], Pterm "cons" [Pterm "dyado" [], Pterm "empty" []]],
     Pterm "cons" [Pterm "lelya" [], Pterm "cons" [Pterm "chicho" [], Pterm "empty" []]],
     JustPvar (pVar "L")]] [])
    prog4


test_prog = [Pfact (Pterm "natNumber" [Pterm "zero" []]),
     Prule (Pterm "natNumber" [Pterm "succ" [JustPvar (pVar "X")]]) [Pterm "natNumber" [JustPvar (pVar "X")]]]
test40 = genn (Node [Pterm "natNumber" [JustPvar (pVar "X")]] [])
    -- genn (Node [Pterm "natNumber" [Pterm "zero" []]] [Just [PLEquation (Pvar "X") (Pterm "zero" [])]])
    test_prog

test41 = genn (Node [Pterm "natNumber" [Pterm "zero" []]] [Just [PLEquation (pVar "X") (Pterm "zero" [])]])
    test_prog

test42 = genn (Node [Pterm "natNumber" [Pterm "succ" [JustPvar (pVar "X")]]] [Just [PLEquation (pVar "X") (Pterm "succ" [JustPvar (pVar "X")])]])
    test_prog

-- Here
test43 = resolve (Node [Pterm "natNumber" [JustPvar (pVar "X")]] []) test_prog

test44 = mergeUnifiers
    [[PLEquation (Pvar {name = "X", label = 0}) (Pterm "zero" [])],
    [PLEquation (Pvar {name = "X", label = 0}) (Pterm "succ" [JustPvar (Pvar {name = "X", label = 0})])]]
