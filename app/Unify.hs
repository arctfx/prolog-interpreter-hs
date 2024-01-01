module Unify where

import Tokenize

data Variable
    = Variable String
    deriving (Show, Eq)

data Pterm = Pterm Term [Pvar] | JustPvar Pvar deriving (Show, Eq)
data Pvar = Pvar Variable deriving (Show, Eq)

-- G - substitution
-- G :: [Equation]
-- G = { x1 ≐ u1, ..., xm ≐ um }
data Equation = Equation Pvar Pterm deriving (Show, Eq)
type Substitution = [Equation]

-- G U {t ≐ t} => G
delete :: Substitution -> Substitution
delete [] = []
delete (eq : eqs) =
    case eq of
        Equation v1 (JustPvar v2) -> if v1 == v2 then delete eqs else eq : delete eqs
        _ -> eq : delete eqs

-- G U { f(s0, ..., sk) ≐ f(t0, ..., tk) } => G U {s0 ≐ t0, ..., sk ≐ tk}
-- decompose :: Substitution -> Substitution
-- decompose [] = []
-- decompose (eq : eqs) =
--     case eq of
--         Equation 

-- -- G U { f(s0, ..., sk) ≐ g(t0, ..., tm) } => ⊥ if f ≠ g or k ≠ m
-- conflict

-- G U { f(s0, ..., sk) ≐ x } => G U { x ≐ f(s0, ..., sk) }
-- swap

-- eliminate

-- check

-- transform :: [AST] -> Substitution
-- transform 
--alg :: [Term] -> Unifier
-- alg [term] = term
-- alg (t : ts) = 



-- data Unifier = Unifier [(Term, Term)] deriving (Show)
-- data Stackframe = Stackframe [(Term, Term)] deriving (Show)
-- 
-- -- data Pterm = Pterm | Pterm Pvar deriving (Show)
-- -- data Pvar = Pvar Var deriving (Show)
-- applyToTerms :: [Term] -> Unifier -> ()
-- applyToTerms [] = ()
-- applyToTerms (t : ts) = case t of
--     (JustVariable (Variable name) -> if name == u then ...
--     _ ->  
-- 
-- 
-- while :: Stackframe -> ()
-- while Stackframe (x : xs) =
--     case x of
--         ((JustVariable var), t2) -> if variableOccurs var t2 then while $ unify t1 t2 else while xs 
--         (t1, (JustVariable var)) -> if variableOccurs var t1 then while $ unify t1 t2 else while xs
--         (x, x) -> while xs -- do nothing
--         (t1, t2) -> if compatible t1 t2 then ... else mgu = Fail
--         _ -> mgu = Fail
-- 
-- pUnify :: Term -> Term -> Unifier
-- pUnify t1 t2 = 
--     mgu = Unifier []
--     stack = Stackframe [(t1, t2)]
--     while stack
-- 
-- 
-- -- occurs check: checks if the right side contains the left side of a rule
-- occurs :: AST -> Bool
-- occurs tr = 
--     case tr of
--         (Rule ls rs) -> contains ls rs where
--             contains :: Atom -> [Atom] -> Bool
--             contains ls [] = False
--             contains ls [r] = ls == r
--             contains ls (r:rs) = if ls == r then True else checkInsides || contains ls rs
--                 where checkInsides :: Atom -> Atom -> Bool
--                     checkInsides (Atom lname lterms) (Atom rname rterms) = ...
-- 
-- apply :: MGU -> Substitution -> MGU
-- 
-- unify :: Program AST -> Query -> MGU
-- unify (Program (st : rest)) query = 
--     mgu = MGU []
--     case st of
--         (Rule ls rs) -> -- if Not occurs ls rs then
--             -- if (ls == rs) then do nothing
--             mgu <> Substitution ls rs
--             ...
--         (Fact atm) -> ...
--         _ -> Fail

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