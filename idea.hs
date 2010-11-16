{-# LANGUAGE PatternGuards #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Applicative
import Data.Either (partitionEithers)

type Name = String
type Record = Map.Map Name AST

data AST 
    = Var Name
    | Record Record
    | Acc AST Name
    | Lub AST AST
    | Star

showRecord :: Record -> String
showRecord r = "{ " ++ intercalate ", " [ x ++ " = " ++ show y | (x,y) <- Map.assocs r ] ++ " }"

instance Show AST where
    show (Var n) = n
    show (Record r) = showRecord r
    show (Acc e n) = show e ++ "." ++ n
    show (Lub a b) = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
    show Star = "*"

-- XXX wrong.  Doesn't respect closure.
eval :: Map.Map Name AST -> AST -> AST
eval env (Var n) | Just x <- Map.lookup n env = eval env x
                 | otherwise = Var n
eval env (Record r) = Record r
eval env (Acc e n) = 
    case eval env e of
        Record r -> eval (r `Map.union` env) (Var n)
        x -> Acc x n
eval env (Lub a b) = 
    case (eval env a, eval env b) of
        (Record r, Record s) -> Record (Map.unionWith Lub r s)
        (Star, y) -> y
        (x, Star) -> x
        (x,y) -> Lub x y
eval env Star = Star