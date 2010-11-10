{-# LANGUAGE PatternGuards #-}

module LP.AST where

import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.Map as Map
import Control.Arrow (first)
import Data.Maybe (listToMaybe)
import Control.Monad.Trans.State
import Control.Monad (forever)
import Control.Monad.Identity
import Control.Applicative

type Name = String

data AST 
    = Lambda Name AST
    | Var Name
    | App AST AST
    deriving (Eq,Ord)

instance Show AST where
    show = showAST

showAST :: AST -> String
showAST = go False False
    where
    go ap lp (Lambda n ast) = parens lp $ "\\" ++ n ++ inL ast
    go ap lp (Var n) = n
    go ap lp (App a b) = parens ap (go False True a ++ " " ++ go True False b)

    inL (Lambda n ast) = " " ++ n ++ inL ast
    inL x = ". " ++ go False False x

    parens True x = "(" ++ x ++ ")"
    parens False x = x

freeVars :: AST -> Set.Set Name
freeVars (Lambda name ast) = Set.delete name (freeVars ast)
freeVars (Var n) = Set.singleton n
freeVars (App a b) = Set.union (freeVars a) (freeVars b)

splits :: [a] -> [([a],[a])]
splits xs = ([], xs) : go xs
    where
    go [] = []
    go (x:xs) = map (first (x:)) (splits xs)

find :: (a -> Bool) -> [a] -> Maybe a
find p = listToMaybe . filter p

nameExtension :: Name -> Name
nameExtension name
    | null name = "@0"
    | Just (nonn, num) <- numeric = nonn ++ show (succ (read num :: Integer))
    | last name == '\'' = name ++ "0"
    where
    numeric = find (all Char.isDigit . snd) (splits name)

nameExtensions :: Name -> [Name]
nameExtensions = tail . iterate nameExtension

validExtension :: Set.Set Name -> Name -> Name
validExtension set = head . filter (not . (`Set.member` set)) . nameExtensions

subst :: Name -> AST -> AST -> AST
subst name with ast = go ast
    where
    go (Lambda n ast)
        | name == n = Lambda n ast
        | n `Set.member` vars = let newName = validExtension vars n in Lambda newName (go (subst n (Var newName) ast))
        | otherwise = Lambda n (go ast)
    go (Var n) | name == n = with
               | otherwise = Var n
    go (App a b) = App (subst name with a) (subst name with b)

    vars = freeVars with

whnf :: AST -> AST
whnf (App f x) = case whnf f of
    Lambda n ast -> whnf (subst n x ast)
    Var n -> App (Var n) x
    App a b -> App (App a b) x
whnf x = x

normalize :: (Name -> AST) -> AST -> AST
normalize env = runIdentity . normalizeM (return . env)

normalizeEnv :: Map.Map Name AST -> AST -> AST
normalizeEnv env = normalize f
    where
    f x | Just e <- Map.lookup x env = e
        | otherwise = Var x

normalizeM :: (Functor m, Monad m) => (Name -> m AST) -> AST -> m AST
normalizeM env (Lambda n ast) = Lambda n <$> normalizeM env ast
normalizeM env (App f x) = do
    f' <- normalizeM env f
    case f' of
        Lambda n ast -> normalizeM env $ subst n x ast   -- call by name
        Var n -> App (Var n) <$> normalizeM env x
        App a b -> App (App a b) <$> normalizeM env x
normalizeM env v@(Var n) = do
    lup <- env n
    if lup /= v then normalizeM env lup else return v
