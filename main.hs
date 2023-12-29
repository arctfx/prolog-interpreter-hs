-- turn warnings into errors
{-# OPTIONS_GHC -Werror #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
--{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
--{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

--import Data.Char (intToDigit)
--import Tokenize
import Data.Char (intToDigit, isAsciiUpper, isAsciiLower, isSpace)
import Prelude hiding (read)

----------------------------------------------------------------
-- MODULE TOKENIZE ---------------------------------------------

data Token
    --
    = StringIdentifier String
    | StringVariable String
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
    | x == '?' = Questionmark
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

----------------------------------------------------------------
-- MODULE MAIN -------------------------------------------------

-- AST
data AST
    = Rule Atom [Atom]
    | Fact Atom
    -- Query
    deriving (Show)

data Term
    = JustConstant Constant
    | JustVariable Variable
    | JustAtom Atom
    deriving (Show)

data Atom
    = Atom Identifier [Term]
    -- Identifier Term [Term]
    deriving (Show)

data Variable
    = Variable String
    deriving (Show)

data Constant
    = Constant Identifier
    deriving (Show)

data Identifier
    = Identifier String
    deriving (Show)

-- comb is a combinator for sequencing operations that return Maybe
--comb :: Maybe a -> (a -> Maybe b) -> Maybe b
--comb Nothing  _ = Nothing
--comb (Just x) f = f x


-- TO-DO: merge combine & combine
-- maybe deprecated
-- combine is an operator that merges two tokens
combine :: Token -> Token -> Token
combine (StringIdentifier str) (LowerLetter ltr) = StringIdentifier $ str ++ [ltr]
combine (StringIdentifier str) (UpperLetter ltr) = StringIdentifier $ str ++ [ltr]
combine (StringIdentifier str) (Numb num) = StringIdentifier $ str ++ [intToDigit num]
combine (StringIdentifier str) (StringVariable var) = StringIdentifier $ str ++ var -- new
combine (StringIdentifier str1) (StringIdentifier str2) = StringIdentifier $ str1 ++ str2 -- new
combine (LowerLetter ltr) (StringIdentifier str)  = StringIdentifier $ ltr : str
combine (LowerLetter ltr) (StringVariable str)  = StringIdentifier $ ltr : str -- new
combine (LowerLetter lltr) (UpperLetter ultr) = StringIdentifier $ lltr : [ultr]
combine (LowerLetter ltr1) (LowerLetter ltr2) = StringIdentifier $ ltr1 : [ltr2]
combine (LowerLetter ltr) (Numb num) = StringIdentifier $ ltr : [intToDigit num]
combine (StringVariable str) (LowerLetter ltr) = StringVariable $ str ++ [ltr]
combine (StringVariable str) (UpperLetter ltr) = StringVariable $ str ++ [ltr]
combine (StringVariable str) (Numb num) = StringVariable $ str ++ [intToDigit num]
combine (StringVariable var) (StringIdentifier str) = StringVariable $ var ++ str -- here
combine (StringVariable var1) (StringVariable var2) = StringVariable $ var1 ++ var2 -- new
combine (UpperLetter ltr) (StringIdentifier str)  = StringVariable $ ltr : str
combine (UpperLetter ltr) (StringVariable str)  = StringVariable $ ltr : str -- new
combine (UpperLetter lltr) (UpperLetter ultr) = StringVariable $ lltr : [ultr]
combine (UpperLetter ltr1) (LowerLetter ltr2) = StringVariable $ ltr1 : [ltr2]
combine (UpperLetter ltr) (Numb num) = StringVariable $ ltr : [intToDigit num]
combine smt None = smt
combine None smt = smt
-- more patterns
combine _ _ = None


-- . . . code . . . 

type LineNumber = Int
data ParseResult a
    = ParseResult [Token] a
    | ParseError LineNumber String
    deriving (Show, Eq)

-- оправя грешката в Parse за инстанция на класа Show..
instance Show (a -> b) where
         show a = "funcion"

-- monad
data Parse a = Parse ([Token] -> ParseResult a) deriving (Show)

-- Parse Monad

instance Functor Parse where
        fmap :: (a -> b) -> Parse a -> Parse b
        fmap f action = do
                f <$> action

instance Applicative Parse where
        (<*>) :: Parse (a -> b) -> Parse a -> Parse b
        (<*>) af action = do
                f <- af
                f <$> action
        pure :: a -> Parse a
        pure = return

instance Monad Parse where
        return :: a -> Parse a
        return x = Parse $ \tokens -> ParseResult tokens x
        (>>=) :: Parse a -> (a -> Parse b) -> Parse b
        (Parse run) >>= f = Parse $ \tokens ->
            case run tokens of
                (ParseResult tokens' x) -> let (Parse run') = f x in
                    case run' tokens' of
                        (ParseError l m) -> ParseError l m
                        res -> res
                (ParseError l m) -> ParseError l m

-- peek?
getToken :: Parse Token
getToken = Parse
    $ \tokens ->
        case tokens of
            [] -> ParseError 0 "Cannot take token!"
            (t:ts) -> ParseResult (t:ts) t

-- read?
accept :: Parse Token
accept = Parse
    $ \tokens ->
        case tokens of
            [] -> ParseError 0 "Cannot take token!"
            t:ts -> ParseResult ts t

expect :: Token -> Parse Token
expect token = Parse
    $ \tokens ->
        case tokens of
            [] -> ParseError 0 "Expected token but no tokens found!"
            t:ts ->
                if t == token
                    then ParseResult ts t
                else ParseError 0 $ "Expected token " ++ show token ++ " but" ++ show t ++ "found" -- here

-- used when we want to read anything even if the list is empty; we don't need to check if the list is empty beforehand
-- similar to accept
read :: Parse Token
read = Parse
    $ \tokens ->
        case tokens of
            [] -> ParseResult [] None
            t:ts -> ParseResult ts t

-- used when we want to see the next token without actually reading it and without throwing exceptions
-- similar to getToken
peek :: Parse Token
peek = Parse
    $ \tokens ->
        case tokens of
            [] -> ParseResult [] None
            t:ts -> ParseResult (t:ts) t

-- used for reading several characters at once
-- a string contains only lowercase and upper letters and digits
readString :: Parse Token
readString = do
    curr <- peek -- read -- accept -- getToken
    case curr of
        None -> return None -- end of list is reached
        LowerLetter l -> do
            curr <- read
            next <- readString
            return $ curr `combine` next
        UpperLetter l -> do
            curr <- read
            next <- readString
            return $ curr `combine` next
        Numb n -> do
            curr <- read
            next <- readString
            return $ curr `combine` next
        _ -> return None -- stops reading otherwise



-- similar to expect?
-- oneOf :: LineNumber, ErrorMessage, Eithers -> Found
oneOf :: Int -> String -> [Parse a] -> Parse a
oneOf line msg possibles = Parse $ \tokens ->
    case possibles of
        [] -> ParseError line "List is empty!"
        (Parse x) : xs -> --func : funcs
            case x tokens of
                (ParseError l _) -> let (Parse x') = oneOf l msg xs in x' tokens
                (ParseResult tkns a) -> ParseResult tkns a --here


cerror :: String -> Parse a
cerror msg = Parse $ \token -> ParseError 0 msg

-- combinators

identifier :: Parse Token
identifier = do
    token <- getToken
    case token of
        LowerLetter l -> do
            curr <- expect $ LowerLetter l
            next <- readString
            case next of
                -- the next 3 lines are for the corner case when the string if 2-lettered
                LowerLetter l -> return $ curr `combine` next
                UpperLetter l -> return $ curr `combine` next
                Numb n -> return $ curr `combine` next
                StringIdentifier s -> return $ curr `combine` next
                _ -> return $ curr `combine` StringIdentifier ""
        _ -> cerror "Cannot parse identifier!"

variable :: Parse Token
variable = do
    token <- getToken
    case token of
        UpperLetter l -> do
            curr <- expect $ UpperLetter l
            next <- readString
            case next of
                LowerLetter l -> return $ curr `combine` next
                UpperLetter l -> return $ curr `combine` next
                Numb n -> return $ curr `combine` next
                StringIdentifier s -> return $ curr `combine` next
                -- StringVariable s -> return $ curr `combine` next -- cause of Issue 2
                _ -> return $ curr `combine` StringVariable ""
        _ -> cerror "Cannot parse variable!"

space :: Parse Token
space = do
    token <- getToken
    case token of
        Space -> do expect Space
        _ -> cerror "Cannot parse space!"

ruleOp :: Parse Token
ruleOp = do
    token <- getToken
    case token of
        Colon -> do
            curr <- expect Colon
            next <- expect Dash
            return RuleOperator
        _ -> cerror "Cannot parse rule operator! Expected a dash after the colon."

queryOp :: Parse Token
queryOp = do
    token <- getToken
    case token of
        Questionmark -> do
            curr <- expect Questionmark
            next <- expect Dash
            return QueryOperator
        _ -> cerror "Cannot parse rule operator! Expected a dash after the question mark."

specCharacter :: Parse Token
specCharacter = do
    token <- getToken
    case token of
        OpenBracket -> expect OpenBracket
        CloseBracket -> expect CloseBracket
        Comma -> expect Comma
        _ -> cerror "Cannot parse special symbol!"

-- Second level parsing

-- parseFact :: Parse AST
-- parseFact = do
--     token <- getToken
--     case token of
--         StringIdentifier s -> do
--             name <- expect $ StringIdentifier s
--             expect Dot
--             return $ Fact Atom name
--         _ -> cerror"Cannot parse fact!"

        
-- Tests
tokenList :: [Token]
tokenList = tokenize "identifier"

--
removespace :: [Token] -> [Token]
removespace [] = []
removespace (Space:l) = removespace l
removespace (x:xs) = x : removespace xs

data Program a = Program [a] deriving (Show)

-- ISSUES
-- ISSUE 1: FIXED: if an identifier string consists of 2 characters then the last character is omitted
-- ISSUE 2: reproduced: if a variable string consists of more than 2 characters then only the first character is saved
--          expection: when the second letter is also uppercase this is not the case

program :: Parse (Program Token)
program = oneOf 0 "Cannot parse program" [do
                                            idnt <- identifier
                                            (Program rest) <- program
                                            return $ Program (idnt : rest),
                                          do
                                            var <- variable
                                            (Program rest) <- program
                                            return $ Program (var : rest),
                                          do 
                                            spc <- space
                                            (Program rest) <- program
                                            return $ Program (spc : rest),
                                          do
                                            rop <- ruleOp
                                            (Program rest) <- program
                                            return $ Program (rop : rest),
                                          do
                                            qop <- queryOp
                                            (Program rest) <- program
                                            return $ Program (qop : rest),
                                          do
                                            spec <- specCharacter
                                            (Program rest) <- program
                                            return $ Program (spec : rest),
                                          return $ Program []
                                         ]

parse :: String -> Parse (Program Token) -> ParseResult (Program Token)
parse str (Parse run) = run $ tokenize str

--parseExpr :: [Token] -> Parse Token 
--parseExpr x = identifier x

-- program :: [AST]

-- query :: Program, Query -> Output
-- query :: [AST] -> [AST] -> String 


--atom :: Parse Atom
--atom = do
--    t <- getToken
--    case t of
--        (Identifier name) -> do
--            expect (Identifier name)
--            expect OpenBracket
--            expr <- term
--            --
--            expect CloseBracket
--            return $ Atom expr
--        _ -> cerror "Cannot parse atom!"

--term :: Parse Term
--term = do
--    t <- getToken
--    case t of
--        (StringConstant s) -> do
--            expect $ StringConstant s
--            return $ StringConstant s
--        (Variable v) -> do
--            expect $ Variable v
--            return $ Variable v
--        (Atom i ts) -> do
--            expect $ Atom i ts
--            --
--            return $ Atom i ts
--        _ -> cerror "Cannot parse term!"

--fact :: Parse Fact
--fact = do
--    t <- getToken
--    case t of
--        (Atom i ts) -> do
--            expect $ Atom i ts
--            expect Dot
--            return $ Atom i ts
--        _ -> cerror "Cannot parse fact!"

--rule :: Parse Rule
--rule = do
--    t <- getToken
--    case t of
--        (Atom i ts) -> do
--            a <- expect $ Atom i ts
--            expect Colon
--            expect Dash
--            b <- expect $ Atom _ _
--            return Rule a [b]
--        _ -> error "Cannot parse rule!"



