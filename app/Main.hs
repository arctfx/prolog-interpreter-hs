{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use <$>" #-}


module Main where

--import Data.Char (intToDigit)
import Tokenize
import Parse hiding (getArgs)
import Data.Char (intToDigit)
import Prelude hiding (read, seq)
import System.IO
import Unify
import Interpret
import System.Environment
import Control.Monad (forever)


main :: IO ()
main =
    let
        interpretQuery db = do
            putStrLn "\nEnter query: "
            qry <- getLine
            let
                node = query qry -- "?- sum(s(s(zero)), s(s(zero)), X)."
                res = resolve node db in
                print res
        in do
            args <- getArgs
            case args of
                [path] -> do
                    db <- interpretFile path
                    forever $ interpretQuery db
                _ -> putStrLn "Error: wrong number of arguments passed to main"




-- finish 
compile :: String -> Maybe (Program AST)
compile str = ast (parse str parser)

-- compiles a file to Program AST
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

-- Note: edited by angel
interpretFile :: String -> IO Database
interpretFile fileName = do
        contents <- readFile fileName
        let
            cmpl :: [String] -> IO [AST]
            cmpl [] = pure []
            cmpl (l : ls) =
                case compile l of
                    Just (Program x) -> do
                        rest <- cmpl ls
                        pure $ x ++ rest
                    Nothing -> do
                        putStrLn "Parsing error has occured! The database may not be complete."
                        pure []
            in do
                ast <- cmpl (filter (not . null) (lines contents))
                return $ astToIR ast

query :: String -> Node
query str =
    case compile str of
        Just (Program x) -> Node [pterm | Pfact pterm <- astToIR x] []
        Nothing -> Node [] [] -- should throw exception
   --  astToIR $ cmpl (filter (not . null) (lines contents))

-- To-dos:
-- -> Revise error handling
-- -> Remove code repetition