module RC.Main where

import System (getArgs)
import System.Console.Readline (readline, addHistory)
import Control.Monad.Identity
import qualified Text.Parsec as P
import qualified Data.Map as Map
import RC.Parser
import Control.Applicative
import RC.AST
import System.IO

run :: String -> String -> IO ()
run fname str = do
    case runIdentity $ P.runParserT (complete expr) () fname str of
        Left err -> print err
        Right x -> print $ eval Map.empty x

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        [] -> interactive
        [file] -> run file =<< readFile file
        _ -> putStrLn "Usage: rc [file]"

interactive :: IO ()
interactive = do
    line <- readline "> "
    case line of
        Nothing -> return ()
        Just s -> addHistory s >> run "<input>" s >> interactive
