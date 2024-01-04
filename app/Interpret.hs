module Interpret where

import Unify

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
type Database = [Pterm]
data Tree = Tree Node [Tree]
data Node = Empty | Node Pterm [Maybe Unifier] -- (Maybe Unifier) 

-- Alg:
-- generates a tree of the children nodes of Node
genn :: Node -> Database -> [Node]
genn (Node term mgu) db = [Node (plUnifierApplyToTerm (head unifier) term) (unifier : mgu) | unifier <- solve term db]
    where
        solve :: Pterm -> Database -> [Maybe Unifier]
        solve term (x : xs) = 
            let 
                u = plUnify term x
            in
                case u of
                    Nothing -> solve term xs
                    Just _ -> u : solve term xs 

genn _ _ = []



-- initial tree: Root -> G
tree = Tree Empty [Tree $ Node g [] $ []]

children :: Tree -> [Node]
children Tree _ [] = []
children Tree _ [Tree (Node node _) _] = [node]
children Tree _ (Tree (Node node _) _ : sts) = node : children sts

-- loop :: [a] -> (a -> b) -> b
-- loop [] f = []
-- loop (x : xs) f = f x : loop xs f

cat :: (a, b) -> ([a], [b]) -> ([a], [b])
cat (x, y) (xs, ys) = cat (x : xs, y : ys)


resolve :: Tree -> [Unifier]
resolve tree =
    loop $ children tree
    where
        loop :: [Node] -> [Unifier]
        loop nodes =
            let
                (gens, sols) = loop2 gen
            in 
                (gens, sols) cat loop1 nodes

-- LOOP1
-- takes nodes, return genn children and solutions
loop1 :: [Node] -> ([Node], [Unifier])
loop1 [] = []
loop1 (node : nodes) nexts =
    let
        -- newTree = add subtree to tree
        (gens, sols) = loop2 genn node
    in 
        (gens, sols) cat loop1 children

-- LOOP2
loop2 :: [Node] -> ([Node], [Unifier])
loop2 [] = []
loop2 (node : nodes) =
    case node of
        Node term Nothing _ -> (Empty, []) -- remove node from nodes = return empty node; no solution found
        Node term (Just []) mgu -> (node, mgu) -- return solution
        _ -> (node, []) cat $ g nodes -- continue; no solution found