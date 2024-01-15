 {-# LANGUAGE InstanceSigs #-}
 
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
{-# HLINT ignore "Use <$>" #-}


module Main where

--import Data.Char (intToDigit)
import Tokenize
import Data.Char (intToDigit)
import Prelude hiding (read, seq)
import System.IO
import Unify
import Interpret


-- dummy 
main :: IO ()
main =
  do
    word <- getLine
    print word

----------------------------------------------------------------
-- MODULE MAIN -------------------------------------------------

-- combine is an operator that merges two tokens
-- ugly but necessary
combine :: Token -> Token -> Token
combine (IdentifierTkn str) (LowerLetter ltr) = IdentifierTkn $ str ++ [ltr]
combine (IdentifierTkn str) (UpperLetter ltr) = IdentifierTkn $ str ++ [ltr]
combine (IdentifierTkn str) (Numb num) = IdentifierTkn $ str ++ [intToDigit num]
combine (IdentifierTkn str) (VariableTkn var) = IdentifierTkn $ str ++ var -- new
combine (IdentifierTkn str1) (IdentifierTkn str2) = IdentifierTkn $ str1 ++ str2 -- new
combine (LowerLetter ltr) (IdentifierTkn str)  = IdentifierTkn $ ltr : str
combine (LowerLetter ltr) (VariableTkn str)  = IdentifierTkn $ ltr : str -- new
combine (LowerLetter lltr) (UpperLetter ultr) = IdentifierTkn $ lltr : [ultr]
combine (LowerLetter ltr1) (LowerLetter ltr2) = IdentifierTkn $ ltr1 : [ltr2]
combine (LowerLetter ltr) (Numb num) = IdentifierTkn $ ltr : [intToDigit num]
combine (VariableTkn str) (LowerLetter ltr) = VariableTkn $ str ++ [ltr]
combine (VariableTkn str) (UpperLetter ltr) = VariableTkn $ str ++ [ltr]
combine (VariableTkn str) (Numb num) = VariableTkn $ str ++ [intToDigit num]
combine (VariableTkn var) (IdentifierTkn str) = VariableTkn $ var ++ str -- here
combine (VariableTkn var1) (VariableTkn var2) = VariableTkn $ var1 ++ var2 -- new
combine (UpperLetter ltr) (IdentifierTkn str)  = VariableTkn $ ltr : str
combine (UpperLetter ltr) (VariableTkn str)  = VariableTkn $ ltr : str -- new
combine (UpperLetter lltr) (UpperLetter ultr) = VariableTkn $ lltr : [ultr]
combine (UpperLetter ltr1) (LowerLetter ltr2) = VariableTkn $ ltr1 : [ltr2]
combine (UpperLetter ltr) (Numb num) = VariableTkn $ ltr : [intToDigit num]
combine smt None = smt
combine None smt = smt
combine smt Space = smt
combine Space smt = smt
-- more patterns
combine _ _ = None

--

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


-- combinators

-- similar to expect?
-- oneOf :: LineNumber, ErrorMessage, Eithers -> Found
oneOf :: Int -> String -> [Parse a] -> Parse a
oneOf line msg possibles = Parse $ \tokens ->
    case possibles of
        [] -> ParseError line "List is empty!"
        (Parse x) : xs -> -- func : funcs
            case x tokens of
                (ParseError l _) -> let (Parse x') = oneOf l msg xs in x' tokens
                (ParseResult tkns a) -> ParseResult tkns a -- here

-- applicative
seq :: Parse (Program Token) -> Parse (Program Token) -> Parse (Program Token)
(Parse p) `seq` (Parse q) = Parse $ \tokens ->
    case p tokens of
        (ParseError l msg) -> ParseError l msg
        (ParseResult list (Program tkns)) -> q tkns

cerror :: String -> Parse a
cerror msg = Parse $ \token -> ParseError 0 msg

-- Parsers

identifier :: Parse Token
identifier = do
    token <- getToken
    case token of
        LowerLetter l -> do
            curr <- expect $ LowerLetter l
            next <- readString
            case next of
                -- the next 3 lines are for the corner case when the string consists of 2 letters
                LowerLetter l -> return $ curr `combine` next
                UpperLetter l -> return $ curr `combine` next
                Numb n -> return $ curr `combine` next
                IdentifierTkn s -> return $ curr `combine` next
                _ -> return $ curr `combine` IdentifierTkn ""
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
                IdentifierTkn s -> return $ curr `combine` next
                -- VariableTkn s -> return $ curr `combine` next -- cause of Issue 2
                _ -> return $ curr `combine` VariableTkn ""
        _ -> cerror "Cannot parse variable!"

space :: Parse Token
space = do
    token <- getToken
    case token of
        Space -> do expect Space
        _ -> cerror "Cannot parse space!"

remspace :: Parse Token
remspace = do
    token <- getToken -- here, getToken returns empty?
    case token of
        Space -> do
            expect Space
            mergeSpace
        _ -> accept
            -- otherwise just read the token
            -- cerror "Cannot parse space!"

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

-- 

-- mergeSpace :: Parse Token -- reads spaces and merges with the next token
mergeSpace :: Parse Token
mergeSpace = do
    curr <- peek -- read -- accept -- getToken
    case curr of
        None -> return None -- end of list is reached
        Space -> do
            curr <- read
            next <- mergeSpace
            return $ curr `combine` next
        _ -> do read -- reads next and returns it instead of the space


-- parseTerm
readTerm :: Parse Token
readTerm = do
    curr <- getToken
    case curr of
        None -> return None -- end of list is reached -- should be deleted
        IdentifierTkn name -> do
            expect $ IdentifierTkn name
            next <- peek
            case next of
                OpenBracket -> do -- the term is an atom; -- do not use . <$>
                    expect OpenBracket
                    args <- getArgs
                    return $ Term $ JustAtom $ Atom name args
                _ -> return $ Term $ JustConstant name -- the term is a constant
        VariableTkn name -> do
            accept -- do not forget to accept
            return $ Term $ JustVariable name -- the term is a variable 
        _ -> cerror "Cannot read term!"

getArgs :: Parse [Term]
getArgs = do
    tkn <- readTerm
    case tkn of
        (Term term) -> do
            next <- peek
            case next of
                Comma -> do
                    expect Comma
                    terms <- getArgs
                    return $ term : terms
                CloseBracket -> do
                    expect CloseBracket
                    return [term]
                _ -> cerror "Cannot read term! Expected at least a closing bracket!"
        _ -> cerror "Expected term but encountered something else!"

-- here
getAtoms :: Parse [Atom]
getAtoms = do
    curr <- getToken
    case curr of
        AtomTkn atom -> do
            expect $ AtomTkn atom
            next <- peek
            case next of
                Dot -> do
                    expect Dot
                    return [atom]
                Comma -> do
                    expect Comma
                    atoms <- getAtoms
                    return $ atom : atoms
                _ -> cerror "Error encountered while parsing atoms!"
        _ -> cerror "Cannot get atoms!"

-- unnecessary
skip :: Parse Token
skip = do accept

-- Note: does not recognize nested atoms, but that is not necessary for the functionality of our parser
parseAtom :: Parse Token
parseAtom = do
    token <- getToken
    case token of
        Term (JustAtom atom) -> do
            accept
            return $ AtomTkn atom
        _ -> cerror "Cannot parse atom!"

parseQuery :: Parse Token
parseQuery = do
    token <- getToken
    case token of
        QueryOperator -> do
            expect QueryOperator
            next <- getToken
            case next of
                (AtomTkn atom) -> do
                    expect $ AtomTkn atom
                    expect Dot
                    return $ QueryTkn atom
                _ -> cerror "Error encountered while parsing query!"
        _ -> cerror "Cannot parse query!"
            
-- parse rules and facts (fact is a short notation for true :- <atom>.)
parseRule :: Parse Token -- AST
parseRule = do
    token <- getToken
    case token of
        AtomTkn atom -> do
            expect $ AtomTkn atom
            next <- getToken
            case next of
                -- None -> return None
                Dot -> do
                    expect Dot
                    return $ FactTkn atom
                RuleOperator -> do
                    expect RuleOperator
                    atoms <- getAtoms
                    return $ RuleTkn atom atoms
                _ -> cerror "Error encountered parsing rule!"
        _ -> cerror "Cannot parse fact!"


--
removespace :: [Token] -> [Token]
removespace [] = []
removespace (Space:l) = removespace l
removespace (x:xs) = x : removespace xs

data Program a = Program [a] deriving (Show)

-- combines symbols into names and operators
parserA :: Parse (Program Token)
parserA = oneOf 0 "Cannot parse program" [do
                                            idnt <- identifier
                                            (Program rest) <- parserA
                                            return $ Program (idnt : rest),
                                          do
                                            var <- variable
                                            (Program rest) <- parserA
                                            return $ Program (var : rest),
                                          do
                                            spc <- space
                                            (Program rest) <- parserA
                                            return $ Program (spc : rest),
                                          do
                                            rop <- ruleOp
                                            (Program rest) <- parserA
                                            return $ Program (rop : rest),
                                          do
                                            qop <- queryOp
                                            (Program rest) <- parserA
                                            return $ Program (qop : rest),
                                          do
                                            spec <- specCharacter
                                            (Program rest) <- parserA
                                            return $ Program (spec : rest),
                                          do -- for dots
                                            curr <- skip
                                            (Program rest) <- parserC
                                            return $ Program (curr : rest),
                                          return $ Program []
                                         ]

-- removes unnecessary empty spaces
parserB :: Parse (Program Token)
parserB = oneOf 0 "Error in parserB"   [ do
                                spc <- remspace
                                (Program rest) <- parserB -- here
                                return $ Program (spc : rest),
                              return $ Program []
                            ]

-- combines tokens into terms
parserC :: Parse (Program Token)
parserC = oneOf 0 "Error in parserC"    [ do
                                            trm <- readTerm -- parseAtom
                                            (Program rest) <- parserC -- here
                                            return $ Program (trm : rest),
                                          do
                                            curr <- skip
                                            (Program rest) <- parserC
                                            return $ Program (curr : rest), 
                                          return $ Program []
                                        ]

-- parse terms into atoms
parserD :: Parse (Program Token)
parserD = oneOf 0 "Error in parserD"    [ do
                                            atm <- parseAtom
                                            (Program rest) <- parserD -- here
                                            return $ Program (atm : rest),
                                          do
                                            curr <- skip
                                            (Program rest) <- parserD
                                            return $ Program (curr : rest), 
                                          return $ Program []
                                        ]

-- parse tokens into a rule or a query
parserE :: Parse (Program Token)
parserE = oneOf 0 "Error in parserE"    [ do
                                            rule <- parseRule
                                            (Program rest) <- parserE -- here
                                            return $ Program (rule : rest),
                                          do
                                            qry <- parseQuery
                                            (Program rest) <- parserE -- here
                                            return $ Program (qry : rest),
                                          return $ Program []
                                        ]

-- sequencing parsers
-- parser
ir :: Parse (Program Token)
ir = (((parserA `seq` parserB) `seq` parserC)  `seq` parserD) `seq` parserE

-- parse program
parse :: String -> Parse (Program Token) -> ParseResult (Program Token)
parse str (Parse run) = run $ tokenize str

-- finish 
compile :: String -> Maybe (Program AST)
compile str = ast (parse str ir)

-- generates AST from tokens
ast :: ParseResult (Program Token) -> Maybe (Program AST)
ast prs =
    case prs of
        (ParseResult stack (Program [token])) ->
            case toAST token of
                (Just res) -> Just $ Program res
                Nothing -> Nothing
            where
                toAST :: Token -> Maybe [AST]
                toAST tkn = -- reads the first (and only) token
                    case tkn of
                        (RuleTkn atom atoms) -> Just [Rule atom atoms] -- [Rule atom rs | rs <- atoms]
                        (FactTkn atom) -> Just [Fact atom]
                        (QueryTkn atom) -> Just [Query atom]
                        _ -> Nothing -- error; needs revision
        _ -> Nothing -- error

-- compiles a fine to Program AST
compileFile :: String -> IO (Maybe (Program AST))
compileFile fileName = do
        contents <- readFile fileName
        let 
            cmpl :: [String] -> [AST]
            cmpl [] = []
            cmpl (l : ls) = 
                case compile l of
                    Just (Program x) -> x ++ cmpl ls
                    Nothing -> []
            ast = cmpl (filter (not . null) (lines contents))
            in return $ Just $ Program ast

-- deprecated
interpretFile :: String -> IO Database
interpretFile fileName = do
        contents <- readFile fileName
        let 
            cmpl :: [String] -> [AST]
            cmpl [] = []
            cmpl (l : ls) = 
                case compile l of
                    Just (Program x) -> x ++ cmpl ls
                    Nothing -> []
            ast = cmpl (filter (not . null) (lines contents))
            in return $ astToIR ast


-- To-dos:
-- -> Note: Compiler stops after lines that cannot be parsed,
--      the already parsed lines remain in the program and the compiling is successful
-- -> Revise error handling
-- -> Remove code repetition