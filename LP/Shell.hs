{-# LANGUAGE TemplateHaskell #-}

module LP.Shell where

import qualified System.IO.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified System.Console.Readline as Readline
import LP.AST
import qualified LP.Parser as P
import qualified Text.Parsec as P
import qualified Data.Accessor.Basic as Acc
import Control.Applicative
import Data.Accessor.Template
import Debug.Trace

data Env = Env {
    envDefns_     :: Map.Map Name AST,
    envKnowledge_ :: Set.Set AST
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
    massage _ ident _ expr = modify . Acc.modify envDefns $ Map.insert ident expr

showCommand :: P.Parser (ShellM ())
showCommand = massage <$> P.tok "show" <*> P.expr
    where
    massage _ expr = do
        defns <- Acc.get envDefns <$> get
        liftIO . print . normalizeEnv defns $ expr

assumeCommand :: P.Parser (ShellM ())
assumeCommand = massage <$> P.tok "assume" <*> P.expr
    where
    massage _ expr = do
        defns <- Acc.get envDefns <$> get
        let normexpr = normalizeEnv defns expr
        liftIO . putStrLn $ "-| " ++ show normexpr
        modify (Acc.modify envKnowledge (Set.insert normexpr))

deduceCommand :: P.Parser (ShellM ())
deduceCommand = massage <$> P.tok "deduce" <*> P.expr
    where
    massage _ expr = do
        defns <- Acc.get envDefns <$> get
        knowledge <- Acc.get envKnowledge <$> get
        let normexpr = normalizeEnv defns expr
        if normexpr `Set.member` knowledge
            then go normexpr
            else liftIO . putStrLn $ "Can't deduce because " ++ show normexpr ++ " is not known.  Did you forget to assume it?"
        
    go expr = do
        knowledge <- Acc.get envKnowledge <$> get
        liftIO . putStrLn $ "|- " ++ show expr
        let done = modify (Acc.modify envKnowledge (Set.insert expr))
        case expr of
            App (App (Var "->") p) q | p `Set.member` knowledge -> go q
            Lambda n e -> do
                maybeLine <- liftIO . Readline.readline $ n ++ " = "
                case maybeLine of
                    Nothing -> done
                    Just "" -> done
                    Just line -> case P.parse (P.complete P.expr) "input" line of
                        Left err -> do
                            liftIO . print $ err
                            go expr
                        Right var -> do
                            defns <- Acc.get envDefns <$> get
                            go . normalizeEnv defns $ subst n var e
            _ -> done

bindingsCommand :: P.Parser (ShellM ())
bindingsCommand = massage <$> P.tok "bindings"
    where
    massage _ = do
        defns <- Acc.get envDefns <$> get
        liftIO $ mapM_ putStrLn [ name ++ " = " ++ show expr | (name, expr) <- Map.assocs defns ]

knowledgeCommand :: P.Parser (ShellM ())
knowledgeCommand = massage <$> P.tok "knowledge"
    where
    massage _ = do
        knowledge <- Acc.get envKnowledge <$> get
        liftIO $ mapM_ print (Set.elems knowledge)

parseCommand :: P.Parser (ShellM ())
parseCommand = P.choice [letCommand, showCommand, assumeCommand, deduceCommand, bindingsCommand, knowledgeCommand]

command :: String -> ShellM ()
command "" = return ()
command input = case P.parse (P.complete parseCommand) "input" input of
    Left err -> liftIO $ print err
    Right cmd -> cmd

runShell = execStateT shell emptyEnv
    where
    emptyEnv = Env Map.empty Set.empty
