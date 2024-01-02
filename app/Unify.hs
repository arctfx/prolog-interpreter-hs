module Unify where

import Tokenize

data Variable
    = Variable String
    deriving (Show, Eq)

data Pquery = Pquery Atom deriving (Show, Eq) 
data Pterm
    = Pterm String [Pterm] -- Pterm Term 
    | JustPvar Pvar deriving (Show, Eq)
    -- consts are functions with zero arguments
data Pvar = Pvar Variable deriving (Show, Eq)

-- G - substitution
-- G :: [Equation]
-- G = { x1 ≐ u1, ..., xm ≐ um }
-- Pvar = Pterm
data Equation = Equation Pterm Pterm deriving (Show, Eq)
data MGU = MGU Substitution
type Substitution = [Equation] -- Unifier
type Database = [AST]

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

-- HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- resolve :: Pquery -> Database -> Maybe Substitution
-- resolve qry [] = Nothing
-- resolve qry (clause : clauses) = 
--     case clause of
--         (Rule ls rs) -> 
--             case unify qry clause of
--                 (MGU subs) -> Just subs
--                 _ -> resolve qry clauses 

type Unifier = Substitution
data Resolvent = Resolvent Pterm deriving (Show)
type Stackframe = [Resolvent]

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
                        if compatible x y then
                            let -- recalculate currStack
                                stack = currStack ++ [PLUnifierFrame (largs!!n) (rargs!!n) | n <- [0..(arity x)-1]]
                            in 
                            loop stack currMgu
                        else Nothing
                    _ -> if x == y then loop currStack currMgu else Nothing
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