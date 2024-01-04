module Unify where

import Tokenize

data Variable
    = Variable String
    deriving (Show, Eq)

data Pquery = Pquery String [Pterm] deriving (Show, Eq)
data Pterm
    = Pterm String [Pterm] -- Pterm Term 
    | JustPvar Pvar deriving (Show, Eq)
    -- consts are functions with zero arguments
data Pvar = Pvar String deriving (Show, Eq)

-- G - substitution
-- G :: [Equation]
-- G = { x1 ≐ u1, ..., xm ≐ um }

-- Test 1:
test1_term = Pterm "a" []
test1_resolvent = Pterm "b" []
test1 = plUnify test1_term test1_resolvent
-- Output: Nothing (cannot be resolved, false)
-- Test 2:
test_term = Pterm "exists" [JustPvar (Pvar "F")]
test_resolvent = Pterm "exists" [Pterm "a" []]
test2 = plUnify test_term test_resolvent
-- Output: Just []
-- Note: should be Just [F = a]

-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
-- NEW ------------------------------------------------------------------------------------------------------------------------------------
-- type Stack = [Frame]
-- data Frame = Frame [Pterm] Pterm Pterm -- resolvents, goal term, compare term / position in db / subDB
-- 
-- plResolve :: Pquery -> Database -> PLUnifierStruct -> Maybe PLUnifierStruct
-- plResolve qry (x : xs) mgu = plInterpret [Frame x qry qry] (x : xs) mgu
-- 
-- data Result = Result Stack Frame | Empty -- argument wrapper, just for the next function
-- 
-- plInterpret :: Stack -> Database -> PLUnifierStruct -> Maybe PLUnifierStruct
-- plInterpret [] db mgu = Nothing -- CHECKED
-- plInterpret (Frame resolvents goal compare : frames) db mgu = 
--     let
--         sStack = (Frame resolvents goal compare : frames)
--         sFrame = Frame resolvents goal compare
--     in
--     loop1 resolvents (Just mgu) sFrame
--     where
--         loop1 :: [Pterm] -> Result -> Frame -> Database -> Maybe PLUnifierStruct
--         loop1 _ Empty _ _ = Nothing
--         loop1 [] res _ _ = Nothing -- should be something 
--         loop1 (t : ts) (Just mgu) sFrame db = 
--             -- $Goal is actually a list that contains also the resolvent of the list
--             let
--                 sGoal = [t]
--             in
--             -- remove the $Goal from the Frame resolvents
--             loop1 ts (loop2 sFrame db) -- ???
--             --If $Frame position is the end of the database $P AND the unification status is not success
--             --  Break the innermost while loop
--             where
--                 -- position in DB / subDB, unification, whole DB - fictive
--                 loop2 :: Database -> Database -> Result -- Maybe PLUnifierStruct
--                 loop2 [] db = Empty
--                 -- loop2 subDB/the position is the head of a suffix of the database
--                 loop2 (x : xs) db =
--                     let sCompare = sFrame in
--                     -- rename vars in sCompare
--                     case plUnify sGoal sCompare of
--                         Just mgu -> 
--                             -- If $Frame database position is not the end of the database
--                             let
--                                 newStack = Frame sGoal goal compare : sStack
--                                 newResolvents = [plUnifierApplyToTerm r | r <- resolvents] ++ [plUnifierApplyToTerm compare]
--                                 newGoal = plUnifierApplyToTerm goal
--                                 newCompare = head db
--                             in
--                             Result newStack $ Frame newResolvents newGoal newCompare
--                             -- break loop2
--                         Nothing -> loop2 xs db -- otherwise continue
-- 
--         
-- 
-- 
-- -- HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- resolve :: Pquery -> Database -> Maybe PLUnifierStruct
-- resolve qry [] = Nothing
-- resolve qry (clause : clauses) = 
--     case clause of
--         (Rule ls rs) -> 
--             case unify qry ls of
--                 Just mgu -> Just mgu
--                     -- resolve qry clauses -- ++ mgu 
--                 Nothing -> Nothing
--         (Fact atom) ->
--             case unify qry atom of
--                 Just mgu -> Just mgu
--                     -- resolve qry clauses -- ++ mgu 
--                 Nothing -> Nothing
--         where
--             unify (Pquery lname largs) (Atom rname rargs) = plUnify (Pterm lname largs) (Pterm rname (termsToPterms rargs))
        

termsToPterms :: [Term] -> [Pterm]
termsToPterms [] = []
termsToPterms (t : ts) =
    case t of
        JustAtom (Atom name args) -> Pterm name (termsToPterms args) : termsToPterms ts
        JustConstant name -> Pterm name [] : termsToPterms ts
        JustVariable name -> JustPvar (Pvar name) : termsToPterms ts

-- Initialise the MGU to an empty unifier
-- Push T1 = T2 to the stack
-- While the stack is not empty
-- 	Pop X = Y from the stack
-- 	       case: X is a variable AND X does not occur in Y
--         	Create unifier U such that X = Y
--                 Apply U to MGU
--                 Add U to MGU
--                 Apply U to stack
--         case: Y is a variable AND Y does not occur in X
--         	Create unifier U such that Y = X
--                 Apply U to MGU
--                 Add U to MGU
--                 Apply U to stack
--         case: X and Y are identical constants or variables
--         	do nothing
--         case: X is of form p(a0,..,an) and Y is of form p(b0,..,bn)
-- 		        For m = 0 to n
--                 	push am = bm to the stack
--         default case:
--         	Failure
-- Return the MGU


-- plstack.h
-- data PLStackFrameStruct = PLStackFrameStruct [PLFrame] deriving (Show, Eq)-- [frame (resolvent, goal)]
-- data PLFrame = PLFrame [Pterm] Pterm deriving (Show, Eq)-- resolvent; goal

-- plterm.h
data PLVariable = PLVariable String deriving (Show, Eq)-- var name
-- type PLCompoundStruct = Fact | Atom

-- deprecated
isCompound :: Pterm -> Bool
isCompound (JustPvar _) = False
isCompound (Pterm _ _) = True

-- deprecated
arity :: Pterm -> Int
arity (Pterm name args) = length args
arity _ = 0

occurs :: Pvar -> Pterm -> Bool
occurs var1 (JustPvar var2) = if var1 == var2 then True else False
occurs var (Pterm name args) = foo var args -- could be done with accumulate and map or even simpler
    where 
        foo var [] = False
        foo var (t : ts) = if occurs var t then True else foo var ts

compatible :: Pterm -> Pterm -> Bool
compatible (Pterm lname largs) (Pterm rname rargs) = if lname == rname && length largs == length rargs
    then True else False

-- plunify.h
type PLUnifierStruct = [PLEquation]
data PLEquation = PLEquation Pvar Pterm deriving (Show, Eq)

plUnify :: Pterm -> Pterm -> Maybe PLUnifierStruct
plUnify t1 t2 =
    -- loop stack mgu
    loop [PLUnifierFrame t1 t2] []
    where
        loop :: PLUnifierStackFrameStruct -> PLUnifierStruct -> Maybe PLUnifierStruct
        loop [] mgu = Just mgu
        loop ((PLUnifierFrame x y) : frames) currMgu =
            let currStack = ((PLUnifierFrame x y) : frames) in
                case (x, y) of
                    (JustPvar v, Pterm n ts) ->
                        if not (occurs v (Pterm n ts)) then
                            let
                                -- u = [PLEquation v y] or u = { X = Y }
                                stack = plUnifierApplyToStack (PLEquation v y) currStack
                                mgu = currMgu ++ plUnifierApplyToUnifier (PLEquation v y) currMgu
                            in
                            loop stack mgu
                        else Nothing
                    (Pterm n ts, JustPvar v) ->
                        if not (occurs v (Pterm n ts)) then
                            let
                                stack = plUnifierApplyToStack (PLEquation v x) currStack
                                mgu = mgu ++ plUnifierApplyToUnifier (PLEquation v x) currMgu
                            in
                            loop stack mgu
                        else Nothing -- ?
                    (Pterm lname largs, Pterm rname rargs) ->
                        -- if x=y is not checked here then we have infinite recursion
                        if x == y then loop frames currMgu else 
                        if compatible x y then
                            let -- recalculate currStack
                                stack = [PLUnifierFrame (largs!!n) (rargs!!n) | n <- [0..(arity x)-1]] ++ currStack 
                            in 
                            loop stack currMgu
                        else Nothing
                    _ -> if x == y then loop frames currMgu else Nothing -- loop currStack currMgu
-- ....

type PLUnifierStackFrameStruct = [PLUnifierFrame] -- frame
data PLUnifierFrame = PLUnifierFrame Pterm Pterm deriving (Show, Eq) -- term1, term2

plUnifierApplyToTerm :: PLEquation -> Pterm -> Pterm
plUnifierApplyToTerm (PLEquation var term) t =
    case t of
        (JustPvar v) -> if v == var then term else t -- apply, swap
        (Pterm name args) ->
            Pterm name [plUnifierApplyToTerm (PLEquation var term) arg | arg <- args]
-- ...


plUnifierApplyToStack :: PLEquation -> PLUnifierStackFrameStruct -> PLUnifierStackFrameStruct
plUnifierApplyToStack _ [] = []
plUnifierApplyToStack u [(PLUnifierFrame term1 term2)] =
    [ PLUnifierFrame (plUnifierApplyToTerm u term1) (plUnifierApplyToTerm u term2) ]
plUnifierApplyToStack u ((PLUnifierFrame term1 term2) : ts) =
    PLUnifierFrame (plUnifierApplyToTerm u term1) (plUnifierApplyToTerm u term2) : plUnifierApplyToStack u ts


plUnifierApplyToUnifier :: PLEquation -> PLUnifierStruct -> PLUnifierStruct
plUnifierApplyToUnifier _ [] = []
plUnifierApplyToUnifier u [PLEquation var term] = [ PLEquation var (plUnifierApplyToTerm u term) ]
plUnifierApplyToUnifier u ((PLEquation var term) : xs) = 
    PLEquation var (plUnifierApplyToTerm u term) : plUnifierApplyToUnifier u xs
-- ....





-- G U {t ≐ t} => G
-- delete

-- G U { f(s0, ..., sk) ≐ f(t0, ..., tk) } => G U {s0 ≐ t0, ..., sk ≐ tk}
-- decompose

-- -- G U { f(s0, ..., sk) ≐ g(t0, ..., tm) } => ⊥ if f ≠ g or k ≠ m
-- conflict

-- G U { f(s0, ..., sk) ≐ x } => G U { x ≐ f(s0, ..., sk) }
-- swap

-- G U {x ≐ t} => G{x ↦ t} U {x ≐ t} -- if x not in args of t 
-- eliminate

-- if x not in args of f 
-- check

-- G{x ↦ t}
-- substitute