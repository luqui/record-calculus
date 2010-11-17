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
        Right x -> do
            let (errs, val) = eval Map.empty x
            case errs of
                [] -> print val
                (e:_) -> putStrLn e

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
        let (errs, val) = eval env e
        case errs of
            [] -> liftIO $ print val
            (e:_) -> liftIO $ putStrLn e
    assign = assignCmd <$> binding
    assignCmd (n,e) = do
        env <- get
        let (errs, val) = eval env' e
            env' = Map.insert n val env
        case errs of
            [] -> put env'
            (e:_) -> liftIO $ putStrLn e
    command = P.try assign <|> inspect
