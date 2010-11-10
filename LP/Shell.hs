{-# LANGUAGE TemplateHaskell #-}

module LP.Shell where

import qualified System.IO.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Console.Readline (readline)
import LP.AST
import qualified LP.Parser as P
import qualified Text.Parsec as P
import qualified Data.Accessor.Basic as Acc
import Control.Applicative
import Data.Accessor.Template

data Env = Env {
    envDefns_     :: Map.Map Name AST,
    envKnowledge_ :: Set.Set AST
}

$( deriveAccessors ''Env )

type ShellM = StateT Env IO

shell :: ShellM ()
shell = do  
    maybeLine <- liftIO $ readline ">\\> "
    case maybeLine of
        Nothing -> liftIO $ putStrLn "" >> return ()
        Just line -> command line >> shell

letCommand :: P.Parser (ShellM ())
letCommand = massage <$> P.tok "let" <*> P.ident <*> P.tok "=" <*> P.expr
    where
    massage _ ident _ expr = modify . Acc.modify envDefns $ Map.insert ident expr

showCommand :: P.Parser (ShellM ())
showCommand = massage <$> P.tok "show" <*> P.expr
    where
    massage _ expr = liftIO . print . normalize $ expr

parseCommand :: P.Parser (ShellM ())
parseCommand = P.choice [letCommand, showCommand]

command :: String -> ShellM ()
command input = case P.parse (P.complete parseCommand) "input" input of
    Left err -> liftIO . print $ err
    Right cmd -> cmd

runShell = execStateT shell emptyEnv
    where
    emptyEnv = Env Map.empty Set.empty
