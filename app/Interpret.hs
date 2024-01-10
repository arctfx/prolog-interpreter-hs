module Interpret where

import Unify
import Tokenize ( AST(..), Atom(..), Term(..) )
import qualified Data.Maybe

-- Input: A Goal and a program P
-- Output: An instance of G that is logical consequence of P

-- resolve :: Tree -> [Unifier]
-- genn :: Node -> [Node]
-- 
-- tree = Tree Empty [Tree $ Node query [] $ []]
-- 
-- resolve tree =
--     solutions []
--     newTree = tree 
--     foreach (child in tree.children)
--         nodes = genn child
--         foreach (node in nodes)
--             if node.unifier is Nothing then
--                 remove node from nodes
--             else if node.unifier is [] then
--                 add node.mgu to solutions
--             else continue
--         newTree = add nodes to newTree
--     return solutions ++ resolve newTree


type Unifier = PLUnifierStruct
data Pclause = Prule Pterm [Pterm] | Pfact Pterm deriving (Show, Eq) -- intermediate representation
type Database = [Pclause] -- [Pterm]
-- data Tree = Tree Node [Tree]
data Node = Empty | Node [Pterm] [Maybe Unifier] -- maybe Empty deprecated?
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



-- Alg:
-- generates a tree of the children nodes of Node
-- foreach pclause{term = body} in DB
--      let u = plUnify query term in
--      case u of
--          Nothing -> continue
--          Just _ -> return Node with u and newQuery = apply u to query
-- goal reduction
genn :: Node -> Database -> Maybe [Node]
genn (Node (term:xs) mgu) db =
    let solved = solve term db in
    case solved of
        [] -> Nothing
        _ -> -- Just [Node [apply unifier t | t <- terms ++ xs] (Just unifier : mgu) | (terms, unifier) <- solved]
            -- HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            Just [Node [t | t <- terms ++ xs] (Just unifier : mgu) | (terms, unifier) <- solved]
    where
        -- rename
        -- apply = plUnifierApplyToTerm??
        apply :: Unifier -> Pterm -> Pterm
        apply [] term = term
        apply (eq : eqs) term = apply eqs (plUnifierApplyToTerm [eq] term)

        -- returns (negative terms, unifier)
        solve :: Pterm -> Database -> [([Pterm], Unifier)]
        solve term [] = []
        solve term (x : xs) =
            case x of
                Prule l r ->
                    case plUnify term l of
                        Nothing -> solve term xs
                        Just [] -> ([], []) : solve term xs
                        Just mgu -> (r, mgu) : solve term xs
                Pfact t ->
                    case plUnify term t of
                        Nothing -> solve term xs
                        Just [] -> ([], []) : solve term xs
                        Just mgu -> ([t], mgu) : solve term xs

genn _ _ = Nothing



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
    maybe [] loop (genn node db)
    -- here, maybe missing corner case here - if node.term is tautology?
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
    if null terms then ([node], [mergeUnifiers (Data.Maybe.catMaybes mgu)]) else -- here
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


-- resolving the unifier list produced by the resolve method
-- left sides are not modified, while all right sides are solved to become terms(not vars)
-- resolveUnifier :: [Unifier] -> [Unifier]
-- resolveUnifier [] = []
-- resolveUnifier (x : xs) = [if  | PLEquation vp tp <- findPure (removeEmpty (x : xs)), PLEquation v t <- x] : resolveUnifier xs
--     where 
--         removeEmpty :: [Unifier] -> [Unifier]
--         removeEmpty (x : xs) =
--             case x of 
--                 [] -> removeEmpty xs -- skip empty unifiers
--                 _ -> x : removeEmpty xs 
--         -- get all PLEquations that are of type X = term(contains no vars)
--         isPure :: Pterm -> Bool
--         isPure (JustPvar _) = False
--         isPure (Pterm name terms) =
--             case terms of
--                 [] -> True
--                 (x : xs) -> isPure x && isPure (Pterm name xs)
--         findPure :: Unifier -> Unifier
--         findPure eq = [PLEquation v t | PLEquation v t <- eq, isPure t]


-- merges unifiers into the last unifier of the stack; returns solution
-- here: could be implemented better?
mergeUnifiers :: [Unifier] -> Unifier
mergeUnifiers [] = [] -- unnecesary?
mergeUnifiers [x] = x
mergeUnifiers (xa : xb : xs) =
    case xa of
        [] -> mergeUnifiers (xb : xs) -- skip empty unifiers
        _ -> mergeUnifiers (plUnifierApplyToUnifier xa xb :xs)

-- HERE, needs renaming?
test101 = resolve (Node [Pterm "proud" [JustPvar (pVar "Z")]] [])
    [Prule (Pterm "proud" [JustPvar (pVar "X")])
        [Pterm "parent" [JustPvar (pVar "X"), JustPvar (pVar "Y")],
         Pterm "newborn" [JustPvar (pVar "Y")]],
     Pfact (Pterm "parent" [Pterm "peter" [], Pterm "ann" []]),
     Pfact (Pterm "newborn" [Pterm "ann" []])]


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

-- HERE, composition is not working
-- test40 = genn (Node [Pterm "f" [Pterm "g" [JustPvar (pVar "X")]]] [])
--     -- genn (Node [Pterm "f" [Pterm "g" []]] [])
--     [Prule (Pterm "f" [Pterm "g" [JustPvar (pVar "X")]]) [Pterm "h" [JustPvar (pVar "X")]],
--      Pfact (Pterm "h" [JustPvar (pVar "X")])]
-- -- ?- f(g(X))
-- -- f(X) :- h(X).
-- -- h(X).

-- TO-DO
-- -> add renaming
-- -> implement isTautology
-- -> implement resolveUnifier
-- -> fix composition