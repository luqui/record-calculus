{-# LANGUAGE PatternGuards #-}

module RC.AST where

import Prelude hiding (mapM)
import qualified Data.Map as Map
import qualified Text.Parsec as P
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Traversable (mapM)
import Control.Applicative

type Name = String
type Record a = Map.Map Name a

data AST 
    = Var P.SourcePos Name
    | Record P.SourcePos (Record AST)
    | Acc P.SourcePos AST Name
    | Lub P.SourcePos AST AST

newtype Value = VRec { getVRec :: Record Value -> Record Value }

fix :: Value -> Record Value
fix (VRec f) = let x = f x in x

valLub :: Value -> Value -> Value
valLub (VRec f) (VRec g) = VRec $ \self -> Map.unionWith valLub (f self) (g self)

type Error = [String]

instance (Monoid w) => Monad ((,) w) where
    return x = (mempty, x)
    ~(w,x) >>= f = let (w', y) = f x in (w `mappend` w', y)

programError :: P.SourcePos -> String -> (Error, Value)
programError pos msg = ([e], error e)
    where
    e = show pos ++ ": " ++ msg

eval :: Record Value -> AST -> (Error, Value)
eval env (Var pos n) | Just x <- Map.lookup n env = (mempty, x)
                     | otherwise = programError pos $ "Variable not in scope: " ++ n
eval env (Record pos r) = (check, val)
    where 
    check = let (err,renv) = mapM (eval env') r
                env' = renv `Map.union` env
            in err
    val = VRec $ \self -> 
        let env' = Map.mapWithKey (\k _ -> self Map.! k) r `Map.union` env in
        Map.map (snd . eval env') r
eval env (Acc pos ast n) = do
    val <- fix <$> eval env ast
    case Map.lookup n val of
        Just x -> (mempty, x)
        Nothing -> programError pos $ "Field not in scope: " ++ n
eval env (Lub pos a b) = valLub <$> eval env a <*> eval env b

-- pretty printing

showRecord :: (Show a) => Record a -> String
showRecord r | null inner = "{}"
             | otherwise  = "{ " ++ unwords inner ++ " }"
    where
    inner = [ x ++ " = " ++ show y | (x,y) <- Map.assocs r ]

instance Show AST where
    show (Var _ n) = n
    show (Record _ r) = showRecord r
    show (Acc _ e n) = show e ++ "." ++ n
    show (Lub _ a b) = "(" ++ show a ++ " \\/ " ++ show b ++ ")"

instance Show Value where
    show = go 0
        where
        go indent val | null inner = "{}"
                      | otherwise = "{\n" ++ unlines inner ++ replicate indent ' ' ++ "}"
            where
            inner = [ replicate indent' ' ' ++ x ++ " = " ++ go indent' y | (x,y) <- Map.assocs r ]
            indent' = indent+2
            r = fix val
