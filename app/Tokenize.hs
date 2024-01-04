 {-# LANGUAGE InstanceSigs #-}
 
module Tokenize
(Token( Term, FactTkn, QueryTkn, RuleTkn,
        AtomTkn, IdentifierTkn, VariableTkn, QueryOperator, RuleOperator, Dot, OpenBracket, CloseBracket, Comma, 
        Numb, LowerLetter, UpperLetter, Space, Colon, Dash, Questionmark, None, Undefined ),
AST( Rule, Query, Fact ),
Term( JustConstant, JustAtom, JustVariable),
Atom(Atom),
toNumb,
isNumb,
tokenizeChar,
tokenize,
clean) where

import Data.Char (isAsciiUpper, isAsciiLower, isSpace)

data Token
    --
    = Term Term
    --
    | FactTkn Atom
    | QueryTkn Atom 
    | RuleTkn Atom [Atom] -- [Term]
    --
    | AtomTkn Atom
    | IdentifierTkn String
    | VariableTkn String
    | QueryOperator
    | RuleOperator
    | Dot
    | OpenBracket
    | CloseBracket
    | Comma
    --
    | Numb Int
    | LowerLetter Char
    | UpperLetter Char
    | Space
    | Colon
    | Dash
    | Questionmark -- maybe used in the future
    | None -- actually a good idea to stick it by
    | Undefined -- for undefined symbols
    deriving (Show, Eq)

-- AST
data AST
    = Rule Atom Atom -- [Atom]
    | Query Atom -- Query
    | Fact Atom
    deriving (Show, Eq)

data Term
    = JustConstant String -- Constant
    | JustVariable String -- Variable
    | JustAtom Atom
    deriving (Eq)

data Atom
    = Atom String [Term] -- [Token]
    -- String [Term]
    -- Identifier Term [Term]
    deriving (Eq)

-- deprecated
-- data Variable
--     = Variable String
--     deriving (Show, Eq)
-- 
-- -- deprecated
-- data Constant
--     = Constant Identifier
--     deriving (Show, Eq)
-- 
-- -- deprecated?
-- data Identifier
--     = Identifier String
--     deriving (Show, Eq)

-- for printing purposes
instance Show Term where
    show :: Term -> String
    show (JustAtom atom) = show atom
    show (JustConstant name) = name
    show (JustVariable name) = name

instance Show Atom where
    show :: Atom -> String
    show (Atom name args) = name ++ "( " ++ show args ++ " )" 

toNumb :: Char -> Int
toNumb ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | otherwise = -1

isNumb :: Char -> Bool
isNumb ch
    | toNumb ch == -1 = False
    | otherwise = True

tokenizeChar :: Char -> Token
tokenizeChar x
    | isNumb x = Numb $ toNumb x
    | isAsciiLower x = LowerLetter x
    | isAsciiUpper x = UpperLetter x
    | isSpace x = Space
    | x == ':' = Colon
    | x == '-' = Dash
    | x == '.' = Dot
    | x == '(' = OpenBracket
    | x == ')' = CloseBracket
    | x == ',' = Comma
    | x == '?' = Questionmark
    | otherwise = Undefined -- undefined characters -- should throw exception

tokenize :: String -> [Token]
tokenize [] = []
tokenize [x] = [tokenizeChar x]
tokenize (ch : chs) = tokenizeChar ch : tokenize chs

-- remove Space and None
-- used after initial parsing
clean :: [Token] -> [Token]
clean [] = []
clean (None : xs) = clean xs
clean (Space : xs) = clean xs
clean (x : xs) = x : clean xs