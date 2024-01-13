module UnitTests where

import Main
import Unify
import Interpret
import Data.Fixed (HasResolution(resolution))

----------------------------------------------------------------------------------------------------------
-- MAIN --------------------------------------------------------------------------------------------------

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

