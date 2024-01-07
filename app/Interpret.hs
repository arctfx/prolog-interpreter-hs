module Interpret where

import Unify
import Tokenize

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
data Node = Empty | Node [Pterm] [Maybe Unifier] -- (Maybe Unifier)
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
                JustConstant name -> Pterm name [] -- constants are functions with zero arguments
                JustVariable name -> JustPvar (Pvar name) 



-- Alg:
-- generates a tree of the children nodes of Node
-- foreach pclause{term = body} in DB
--      let u = plUnify query term in
--      case u of
--          Nothing -> continue
--          Just _ -> return Node with u and newQuery = apply u to query 
genn :: Node -> Database -> [Node]
genn (Node (term:xs) mgu) db = [Node [apply unifier t | t <- terms ++ xs] (Just unifier : mgu) | (terms, unifier) <- solve term db]
    where
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

genn _ _ = []



-- initial tree: Root -> G
-- tree = Tree Empty [Tree $ Node g [] $ []]

-- children :: Tree -> [Node]
-- children (Tree _ []) = []
-- children (Tree _ [Tree node _]) = [node]
-- children (Tree _ (Tree node _ : sts)) = node : [nd | Tree nd _ <- sts]

-- loop :: [a] -> (a -> b) -> b
-- loop [] f = []
-- loop (x : xs) f = f x : loop xs f

cat :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
cat (x, y) (xs, ys) = (x ++ xs, y ++ ys)


resolve :: Node -> Database -> [Unifier]
resolve node db =
    snd $ loop $ genn node db
    where
        loop :: [Node] -> ([Node], [Unifier])
        loop [] = ([], [])
        loop (node : nodes) =
            let
                (gens, sols) = loop2 $ genn node db
            in
                (gens, sols) `cat` (loop nodes)

-- LOOP1
-- takes nodes, return genn children and solutions
-- loop1 :: [Node] -> ([Node], [Unifier])
-- loop1 [] = []
-- loop1 (node : nodes) =
--     let
--         -- newTree = add subtree to tree
--         (gens, sols) = loop2 $ genn node
--     in 
--         (gens, sols) `cat` loop1 children

-- LOOP2
-- check all children for solutions
loop2 :: [Node] -> ([Node], [Unifier])
loop2 [] = ([], [])
loop2 (node : nodes) =
    case node of
        Node term (Nothing : _) -> ([], []) `cat` loop2 nodes -- remove node from nodes; no solution found
        Node term (Just [] : mgu) -> ([node], [u | Just u <- mgu]) `cat` loop2 nodes-- return solution
        _ -> ([node], []) `cat` loop2 nodes -- continue; no solution found


