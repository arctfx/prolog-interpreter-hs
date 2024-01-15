{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Interpret where

import Unify
import Tokenize ( AST(..), Atom(..), Term(..) )
import qualified Data.Maybe


type Unifier = PLUnifierStruct
data Pclause = Prule Pterm [Pterm] | Pfact Pterm deriving (Show, Eq) -- intermediate representation
type Database = [Pclause] -- [Pterm]
-- data Tree = Tree Node [Tree]
data Node = Empty | Node [Pterm] [Maybe Unifier] -- maybe Empty is deprecated?
    deriving (Show, Eq)

astToIR :: [AST] -> Database
astToIR [] = []
astToIR (x : xs) =
    case x of
        Rule atom atoms -> Prule (atomToPterm atom) [atomToPterm a | a <- atoms] : astToIR xs
        Fact atom -> Pfact (atomToPterm atom) : astToIR xs
        Query atom -> Pfact (atomToPterm atom) : astToIR xs -- for completeness
    where
        atomToPterm :: Atom -> Pterm
        atomToPterm atom = termToPterm (JustAtom atom)
        termToPterm :: Term -> Pterm
        termToPterm t =
            case t of
                JustAtom (Atom name args) -> Pterm name [termToPterm t | t <- args]
                JustConstant name -> Pterm name [] -- constants are functions(terms) with zero arguments
                JustVariable name -> JustPvar (pVar name)

-- queryToNode :: Database -> Node
-- queryToNode [] = [] 

-- Alg: generates a tree of the children nodes of Node
-- foreach pclause{term = body} in DB
--      let u = plUnify query term in
--      case u of
--          Nothing -> continue
--          Just _ -> return Node with u and newQuery = apply u to query
-- goal reduction
genn :: Node -> Database -> Maybe [Node]
genn (Node (term:xs) mgu) db =
    let solved = solve term xs db
        (Pterm l r) = term
    in
    case solved of
        [] -> Nothing
        _ -> Just [Node terms (Just unifier : mgu) | (terms, unifier) <- solved]
    where
        -- apply filtered unifier to the right side of rule
        -- arguments: unifier; left side of rule; right side of rule
        apply :: Unifier -> Pterm -> [Pterm] -> [Pterm]
        apply unifier (Pterm l r) terms =
            fmap (plUnifierApplyToTerm (filter (pred r) unifier)) terms
            where
                pred :: [Pterm] -> PLEquation -> Bool
                pred [] _ = False
                pred (arg : args) (PLEquation var r) =
                    if occurs var r then False else -- if should be outside of loop
                    case arg of
                        Pterm _ _ -> occurs var arg || pred args (PLEquation var r)
                        JustPvar pvar -> pvar == var || pred args (PLEquation var r)

                occurs :: Pvar -> Pterm -> Bool
                occurs var1 (JustPvar var2) = var1 == var2
                occurs var (Pterm name args) = foo var args -- could be implemented with prelude functions instead; maybe use accomulate or map?
                    where
                        foo var [] = False
                        foo var (t : ts) = occurs var t || foo var ts


        -- returns (negative terms, unifier)
        -- unify term with left side of rule
        -- apply filtered unifier to term and others
        solve :: Pterm -> [Pterm] -> Database -> [([Pterm], Unifier)]
        solve term others [] = []
        solve term others (x : xs) =
            case x of
                Prule l r ->
                    case plUnify term l of
                        Nothing -> solve term others xs
                        Just [] -> (others, []) : solve term others xs
                        Just mgu -> (apply mgu l (r ++ others), mgu) : solve term others xs -- here, used to be (r, mgu)
                Pfact t ->
                    case plUnify term t of
                        Nothing -> solve term others xs
                        Just [] -> (others, []) : solve term others xs
                        Just mgu -> (t : fmap (plUnifierApplyToTerm mgu) others, mgu) : solve term others xs -- here, maybe use apply?

genn _ _ = Nothing


-- maybe use prelude functions instead
cat :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
cat (x, y) (xs, ys) = (x ++ xs, y ++ ys)

-- isTautology :: Pterm -> Bool


-- returns:
-- [] = failed, or tautology, or any solution
-- [unifier0,..] = success
-- should return:
-- Nothing -> "no"
-- isTautology == true -> "yes"
-- [] -> "any"
-- non-empty unifier -> {...}
resolve :: Node -> Database -> [Unifier]
resolve node db =
    fmap (limit node) -- limits only the variables we are looking for as solution
    (maybe [] loop (genn node db))
    -- maybe missing corner case - if node.term is tautology?
    where
        loop :: [Node] -> [Unifier]
        loop [] = []
        loop (node : nodes) =
            -- here, maybe missing corner case here, or maybe not?
            case genn node db of
                Nothing -> loop nodes
                Just gen ->
                    let (gens, sols) = loop1 gen db in
                    sols ++ loop (nodes ++ gens)

        -- LOOP1
        -- takes nodes, return genn children and solutions
        loop1 :: [Node] -> Database -> ([Node], [Unifier])
        loop1 [] db = ([], []) -- 
        loop1 (node : nodes) db =
            -- corner case: node has empty terms
            let (Node terms mgu) = node in
            if null terms then ([node], [mergeUnifiers (Data.Maybe.catMaybes mgu)]) else -- here, fix mergeUnifiers
            -- standard case:
            case genn node db of
                Nothing -> loop1 nodes db
                Just gen -> loop2 gen `cat` loop1 nodes db

        -- LOOP2
        -- check all children for solutions
        loop2 :: [Node] -> ([Node], [Unifier])
        loop2 [] = ([], [])
        loop2 (node : nodes) =
            case node of
                Node terms (Nothing : _) -> ([], []) `cat` loop2 nodes -- remove node from nodes; no solution found
                -- Node terms (Just [] : mgu) -> ([node], [u | Just u <- mgu]) `cat` loop2 nodes-- return solution
                -- HERE
                Node [] mgu -> ([node], [mergeUnifiers (Data.Maybe.catMaybes mgu)]) `cat` loop2 nodes-- return solution -- here, maybe replave [node] with []
                _ -> ([node], []) `cat` loop2 nodes -- continue; no solution found

        -- removes unneccessary equations from unifier
        -- in general node will have only one term
        limit :: Node -> Unifier -> Unifier
        limit (Node terms _) unifier = 
            let pvars = concat [args | Pterm _ args <- terms] in
            plUnifierApplyToUnifier unifier [PLEquation var (JustPvar var) | JustPvar var <- pvars]


-- merges unifiers into the last unifier of the stack; returns solution
-- here: could be implemented better?
-- here: alternative: unifiers keep empty substitutions (X = X)
-- alg1: [Unifier] -> Unifier
-- unifier_list = (θx : θs) //foreach θx in θs
-- if x == 1 then return θx else
-- foreach eq in θx:
--      foreach θ in θs:
--          foreach eq' in θ:
--              if eq'.right is JustPvar and eq.var == eq'.right:
--                  eq'.right = eq.right
--                  break
--          break

-- note: can be optimized
mergeUnifiers :: [Unifier] -> Unifier
mergeUnifiers [] = []
mergeUnifiers [x] = x
mergeUnifiers (xa : xb : xs) =
    case (xa, xb) of
        ([], xb) -> mergeUnifiers (xb : xs) -- skip empty unifiers
        (xa, []) -> mergeUnifiers (xa : xs) -- skip empty unifiers 
        _ ->
            let u = plUnifierApplyToUnifier xa xb in
                -- [[PLEquation va t | (PLEquation va t) <- xa, (PLEquation vy _) <- u, va /= vy ]]
                -- [xa]
            mergeUnifiers (concat (u : [[PLEquation va t | (PLEquation va t) <- xa, (PLEquation vy _) <- u, va /= vy ]]) : xs)


testmerge = plUnifierApplyToUnifier
    [PLEquation (pVar "N") (Pterm "s" [Pterm "s" [JustPvar (pVar "zero")]]),
     PLEquation (pVar "M") (Pterm "zero" []),
     PLEquation (pVar "K") (Pterm "s" [JustPvar (pVar "K")])]
    [PLEquation (pVar "K") (Pterm "s" [Pterm "s" [Pterm "zero" []]]),
     PLEquation (pVar "N") (Pterm "s" [Pterm "s" [Pterm "zero" []]])]

prog5 =
    [Pfact (Pterm "sum" [JustPvar (pVar "N"), Pterm "z" [], JustPvar (pVar "N")]),
     Prule (Pterm "sum" [JustPvar (pVar "N"), Pterm "s" [JustPvar (pVar "M")], Pterm "s" [JustPvar (pVar "K")]])
           [Pterm "sum" [JustPvar (pVar "N"), JustPvar (pVar "M"), JustPvar (pVar "K")]]]

test170 = resolve (Node [Pterm "sum" [Pterm "s" [Pterm "s" [Pterm "z" []]], Pterm "s" [Pterm "s" [Pterm "z" []]], JustPvar (pVar "X")]] [])
    prog5


prog6 =
    [Pfact (Pterm "parent" [Pterm "pesho" [], Pterm "gosho" []]),
     Pfact (Pterm "parent" [Pterm "gosho" [], Pterm "ivan" []]),
     Pfact (Pterm "parent" [Pterm "ivan" [], Pterm "penka" []]),
     Pfact (Pterm "parent" [Pterm "penka" [], Pterm "asen" []]),
     Pfact (Pterm "ancestor" [JustPvar (pVar "X"), JustPvar (pVar "X")]),
     Prule (Pterm "ancestor" [JustPvar (pVar "X"), JustPvar (pVar "Z")])
           [Pterm "parent" [JustPvar (pVar "X"), JustPvar (pVar "Y")], Pterm "ancestor" [JustPvar (pVar "Y"), JustPvar (pVar "Z")]]]

test161 = resolve (Node [Pterm "ancestor" [Pterm "gosho" [], JustPvar (pVar "Y")]] []) prog6

--test_1 = plUnifierApplyToUnifier [PLEquation (pVar "Z") (Pterm "ivan" []), PLEquation (pVar "X") (Pterm "ivan" [])]
--    [PLEquation]