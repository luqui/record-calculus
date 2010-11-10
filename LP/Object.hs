{-# LANGUAGE PatternGuards #-}

module LP.Object where

import qualified Data.Map as Map
import Control.Monad.Trans.Writer
import LP.AST
import Data.List (intercalate)

type Definitions = Map.Map Name Object

data Object = Object 
    { objDefns :: Definitions
    , objParent :: Maybe Object
    , objAST :: AST
    }
    deriving (Eq, Ord)

instance Show Object where
    show = show . objAST

fromAST :: Definitions -> AST -> Object
fromAST env = toObject . runWriter . normalizeM envLookup
    where
    toObject (ast,defns) = Object defns Nothing ast
    envLookup x | Just e <- Map.lookup x env = tell (Map.singleton x e) >> return (objAST e)
                | otherwise                  = return (Var x)

extend :: Definitions -> Object -> Object
extend env p = obj { objParent = Just p }
    where
    obj = fromAST (Map.union env (allDefns p)) (objAST p)

allDefns :: Object -> Definitions
allDefns = Map.unions . map objDefns . ancestry

ancestry :: Object -> [Object]
ancestry obj = obj : maybe [] ancestry (objParent obj)

showDetailed :: Object -> String
showDetailed obj = lhs ++ " |- " ++ show (objAST obj)
    where 
    lhs = intercalate ", " . map shower . Map.toList . objDefns $ obj
    shower (k,v) = k ++ " = " ++ show v
