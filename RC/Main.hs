module RC.Main where

import System (getArgs)
import System.Console.Readline (readline, addHistory)
import Data.Functor.Identity
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad (unless)
import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified Data.Map as Map
import Control.Applicative
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
        [] -> evalStateT interactive Map.empty
        [file] -> run file =<< readFile file
        _ -> putStrLn "Usage: rc [file]"

interactive :: StateT (Map.Map Name Value) IO ()
interactive = do
    line <- liftIO $ readline "> "
    case line of
        Nothing -> return ()
        Just s -> do
            unless (all Char.isSpace s) $ do
                liftIO $ addHistory s 
                case runIdentity $ P.runParserT (complete command) () "<input>" s of
                    Left err -> liftIO $ print err
                    Right action -> action
            interactive
    where
    inspect = inspectCmd <$> expr
    inspectCmd e = do
        env <- get
        liftIO . print $ eval env e
    assign = assignCmd <$> binding
    assignCmd (n,v) = do
        env <- get
        let env' = Map.insert n (eval env' v) env
        put env'
    command = P.try assign <|> inspect
