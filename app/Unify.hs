{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Unify where

import Tokenize

data Variable = Variable String
  deriving (Show, Eq)

data Pquery = Pquery String [Pterm] deriving (Show, Eq)
data Pterm
    = Pterm String [Pterm] -- Pterm Term 
    | JustPvar Pvar deriving (Eq)
    -- consts are functions with zero arguments
data Pvar = Pvar {name :: String, label :: Integer} deriving (Eq) -- label is deprecated
-- def constructor
pVar :: String -> Pvar
pVar name = Pvar name 0

-- ...

-- deprecated
-- data PLStackFrameStruct = PLStackFrameStruct [PLFrame] deriving (Show, Eq)-- [frame (resolvent, goal)]
-- data PLFrame = PLFrame [Pterm] Pterm deriving (Show, Eq)-- resolvent; goal

-- deprecated
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
occurs _ _ = False
-- occurs :: Pvar -> Pterm -> Bool
-- occurs var1 (JustPvar var2) = var1 == var2
-- occurs var (Pterm name args) = foo var args -- could be done with accumulate and map or even simpler
--     where
--         foo var [] = False
--         foo var (t : ts) = occurs var t || foo var ts

compatible :: Pterm -> Pterm -> Bool
compatible (Pterm lname largs) (Pterm rname rargs) = lname == rname && length largs == length rargs

-- plunify
type PLUnifierStruct = [PLEquation]
data PLEquation = PLEquation Pvar Pterm deriving (Eq) -- PLSubstitution

-- for printing purposes
instance Show PLEquation where
    -- show :: PLEquation -> String
    show (PLEquation var term) = show var ++ " = " ++ show term

instance Show Pterm where
    -- show :: Pterm -> String
    show (Pterm name []) = name
    show (Pterm name (x : xs)) = name ++ "(" ++ show x ++ loop xs ++ ")"
        where 
            loop :: [Pterm] -> String
            loop [] = ""
            loop [x] = ", " ++ show x 
            loop (x : xs) = ", " ++ show x ++ loop xs  
    show (JustPvar var) = show var

instance Show Pvar where
    -- show :: Pvar -> String
    show (Pvar name _) = name 


plUnify :: Pterm -> Pterm -> Maybe PLUnifierStruct
plUnify t1 t2 =
    -- loop stack mgu
    loop [PLUnifierFrame t1 t2] []
    where
        loop :: PLUnifierStackFrameStruct -> PLUnifierStruct -> Maybe PLUnifierStruct
        loop [] mgu = Just mgu
        loop ((PLUnifierFrame x y) : currStack) currMgu =
            case (x, y) of
                (JustPvar v, Pterm n ts) ->
                    if not (occurs v (Pterm n ts)) then
                        let
                            u = [PLEquation v y] -- or u = { X = Y }
                            stack = plUnifierApplyToStack u currStack
                            mgu = u ++ plUnifierApplyToUnifier u currMgu
                        in
                        loop stack mgu
                    else Nothing
                (Pterm n ts, JustPvar v) ->
                    if not (occurs v (Pterm n ts)) then
                        let
                            u = [PLEquation v x] -- or u = { Y = X }
                            stack = plUnifierApplyToStack u currStack
                            mgu = u ++ plUnifierApplyToUnifier u currMgu
                        in
                        loop stack mgu
                    else Nothing -- ?
                (Pterm lname largs, Pterm rname rargs) ->
                    -- if x=y is not checked here then we have infinite recursion
                    if x == y then loop currStack currMgu else
                    if compatible x y then
                        let -- recalculate currStack
                            stack = [PLUnifierFrame (largs!!n) (rargs!!n) | n <- [0..arity x - 1]] ++ currStack
                        in
                        loop stack currMgu
                    else Nothing
                (JustPvar vx, JustPvar vy) ->
                    if x == y then loop currStack currMgu 
                    else loop currStack (PLEquation vx y : currMgu) -- newest; used to be Nothing
-- ....

type PLUnifierStackFrameStruct = [PLUnifierFrame] -- frame
data PLUnifierFrame = PLUnifierFrame Pterm Pterm deriving (Show, Eq) -- term1, term2

plUnifierApplyToTerm :: PLUnifierStruct -> Pterm -> Pterm
plUnifierApplyToTerm [] t = t
plUnifierApplyToTerm ((PLEquation var term) : unifier)  t =
    case t of
        (JustPvar v) -> if v == var then plUnifierApplyToTerm unifier term else plUnifierApplyToTerm unifier t -- apply, swap
        (Pterm name args) ->
            plUnifierApplyToTerm unifier (Pterm name [plUnifierApplyToTerm (PLEquation var term : unifier) arg | arg <- args])


plUnifierApplyToStack :: PLUnifierStruct -> PLUnifierStackFrameStruct -> PLUnifierStackFrameStruct
plUnifierApplyToStack [] _ = []
plUnifierApplyToStack _ [] = []
plUnifierApplyToStack u ((PLUnifierFrame term1 term2) : ts) =
    PLUnifierFrame (plUnifierApplyToTerm u term1) (plUnifierApplyToTerm u term2) : plUnifierApplyToStack u ts


plUnifierApplyToUnifier :: PLUnifierStruct -> PLUnifierStruct -> PLUnifierStruct
plUnifierApplyToUnifier [] _ = []
plUnifierApplyToUnifier _ [] = []
plUnifierApplyToUnifier u ((PLEquation var term) : unifier) =
    PLEquation var (plUnifierApplyToTerm u term) : plUnifierApplyToUnifier u unifier
-- ....
