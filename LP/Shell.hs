{-# LANGUAGE TemplateHaskell #-}

module LP.Shell where

import qualified System.IO.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified System.Console.Readline as Readline
import LP.AST
import LP.Object
import qualified LP.Parser as P
import qualified Text.Parsec as P
import qualified Data.Accessor.Basic as Acc
import Control.Applicative
import Data.Accessor.Template
import Debug.Trace

data Env = Env {
    envDefns_     :: Definitions
}

$( deriveAccessors ''Env )

type ShellM = StateT Env IO

shell :: ShellM ()
shell = do  
    maybeLine <- liftIO $ Readline.readline ">\\> "
    case maybeLine of
        Nothing -> liftIO $ putStrLn "" >> return ()
        Just line -> liftIO (Readline.addHistory line) >> command line >> shell

letCommand :: P.Parser (ShellM ())
letCommand = massage <$> P.tok "let" <*> P.ident <*> P.tok "=" <*> P.expr
    where
    massage _ ident _ expr = do
        defns <- Acc.get envDefns <$> get
        let object = fromAST defns expr
        modify . Acc.modify envDefns $ Map.insert ident object

showCommand :: P.Parser (ShellM ())
showCommand = massage <$> P.tok "show" <*> P.expr
    where
    massage _ expr = do
        defns <- Acc.get envDefns <$> get
        liftIO . print . fromAST defns $ expr

bindingsCommand :: P.Parser (ShellM ())
bindingsCommand = massage <$> P.tok "bindings"
    where
    massage _ = do
        defns <- Acc.get envDefns <$> get
        liftIO $ mapM_ putStrLn [ name ++ " = " ++ show expr | (name, expr) <- Map.assocs defns ]

assumptionsCommand :: P.Parser (ShellM ())
assumptionsCommand = massage <$> P.tok "assumptions" <*> P.ident
    where
    massage _ ident = do
        defns <- Acc.get envDefns <$> get
        case Map.lookup ident defns of
            Nothing -> liftIO . putStrLn $ "No such symbol"
            Just obj -> liftIO . mapM_ putStrLn . map showDetailed . ancestry $ obj

parseCommand :: P.Parser (ShellM ())
parseCommand = P.choice [letCommand, showCommand, bindingsCommand, assumptionsCommand]

command :: String -> ShellM ()
command "" = return ()
command input = case P.parse (P.complete parseCommand) "input" input of
    Left err -> liftIO $ print err
    Right cmd -> cmd

runShell = execStateT shell emptyEnv
    where
    emptyEnv = Env Map.empty
