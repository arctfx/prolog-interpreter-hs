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
        node = Node [Pterm "proud" [JustPvar (Pvar "Z")]] []
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

test_term = Pterm "exists" [JustPvar (Pvar "F")]
test_resolvent = Pterm "exists" [Pterm "a" []]
test22 = plUnify test_term test_resolvent
-- Output: Just [F = a]

test23 = plUnifierApplyToUnifier [PLEquation (Pvar "Y") (Pterm "a" [])] [PLEquation (Pvar "X") (Pterm "b" [JustPvar (Pvar "Y")])]
-- Ouput: [PLEquation X = b(a)]

test24 = plUnifierApplyToTerm [PLEquation (Pvar "X") (Pterm "a" []), PLEquation (Pvar "Y") (Pterm "b" []), PLEquation (Pvar "Z") (Pterm "c" [])]
    (Pterm "term" [JustPvar (Pvar "X"), JustPvar (Pvar "X"), JustPvar (Pvar "Z")])
-- Output: Pterm "term" [Pterm "a" [], Pterm "a" [], JustPvar (Pvar "Z")]

test26 = plUnify (Pterm "proud" [JustPvar (Pvar "Z")]) (Pterm "proud" [JustPvar (Pvar "X")])
-- Just [Z = X]


----------------------------------------------------------------------------------------------------------
-- INTERPRET ---------------------------------------------------------------------------------------------


test32 = genn (Node [Pterm "proud" [JustPvar (Pvar "Z")]] [])
    [Prule (Pterm "proud" [JustPvar (Pvar "X")])
        [Pterm "parent" [JustPvar (Pvar "X"), JustPvar (Pvar "Y")],
         Pterm "newborn" [JustPvar (Pvar "Y")]],
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
        node = Node [Pterm "natNumber" [JustPvar (Pvar "X")]] []
        res = resolve node db
        in return res

--



test50 = plUnify (Pterm "natNumber" [JustPvar (Pvar "X")])  (Pterm "natNumber" [Pterm "zero" []])
-- {X = zero}
test51 = plUnify (Pterm "natNumber" [JustPvar (Pvar "X")])  (Pterm "natNumber" [Pterm "succ" [JustPvar (Pvar "X")]])
-- Nothing


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
