module RC.Main where

import System (getArgs)
import System.Console.Readline (readline, addHistory)
import Control.Monad.Identity
import qualified Text.Parsec as P
import qualified Data.Map as Map
import System.IO
import RC.Parser
import RC.AST

run :: String -> String -> IO ()
run fname str = do
    case runIdentity $ P.runParserT (complete expr) () fname str of
        Left err -> print err
        Right x -> print $ eval Map.empty x

main :: IO ()
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