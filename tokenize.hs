module Tokenize where
import Data.Char (isAsciiUpper, isAsciiLower, isSpace)

data Token
    --
    = StringIdentifier String
    | QueryOperator -- unused -- maybe delete
    | RuleOperator -- unused -- maybe delete
    | Dot
    | OpenBracket
    | CloseBracket
    | Comma
    --
    | StringConstant String -- unused -- maybe delete
    | Numb Int
    | LowerLetter Char
    | UpperLetter Char
    | Space
    | Colon
    | Dash
    | Questionmark -- maybe used in the future
    | None -- should be deprecated
    deriving (Show, Eq)

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
    | otherwise = None -- undefined characters

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