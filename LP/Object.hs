{-# LANGUAGE PatternGuards #-}

module LP.Object where

import qualified Data.Map as Map
import Control.Monad.Trans.Writer
import LP.AST
import Data.List (intercalate)

type Definitions = Map.Map Name Object

data Object = Object 
    { objDefns :: Definitions
    , objAST :: AST
    }
    deriving (Eq, Ord)

instance Show Object where
    show = show . objAST

fromAST :: Definitions -> AST -> Object
fromAST env = toObject . runWriter . normalizeM envLookup
    where
    toObject (ast,defns) = Object defns ast
    envLookup x = do
        maybe (return ()) (tell . Map.singleton x) $ Map.lookup x env
        return (Var x)

normalizeObject :: Definitions -> Object -> AST
normalizeObject external obj = normalize envLookup (objAST obj)
    where
    env = objDefns obj `Map.union` external
    envLookup x | Just o <- Map.lookup x env = normalizeObject env o
                | otherwise = Var x

showDetailed :: Object -> String
showDetailed obj = lhs ++ " |- " ++ show (objAST obj)
    where 
    lhs = intercalate ", " . map shower . Map.toList . objDefns $ obj
    shower (k,v) = k ++ " = " ++ show v
