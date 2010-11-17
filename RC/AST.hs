{-# LANGUAGE PatternGuards #-}

module RC.AST where

import qualified Data.Map as Map

type Name = String
type Record a = Map.Map Name a

data AST 
    = Var Name
    | Record (Record AST)
    | Acc AST Name
    | Lub AST AST

newtype Value = VRec { getVRec :: Record Value -> Record Value }

fix :: Value -> Record Value
fix (VRec f) = let x = f x in x

valLub :: Value -> Value -> Value
valLub (VRec f) (VRec g) = VRec $ \self -> Map.unionWith valLub (f self) (g self)

eval :: Record Value -> AST -> Value
eval env (Var n) | Just x <- Map.lookup n env = x
                 | otherwise = error $ "Variable not in scope: " ++ n
eval env (Record r) = VRec $ \self -> 
    let env' = Map.mapWithKey (\k _ -> self Map.! k) r `Map.union` env in
    Map.map (eval env') r
eval env (Acc ast n) = fix (eval env ast) Map.! n
eval env (Lub a b) = valLub (eval env a) (eval env b)


-- pretty printing

showRecord :: (Show a) => Record a -> String
showRecord r | null inner = "{}"
             | otherwise  = "{ " ++ unwords inner ++ " }"
    where
    inner = [ x ++ " = " ++ show y | (x,y) <- Map.assocs r ]

instance Show AST where
    show (Var n) = n
    show (Record r) = showRecord r
    show (Acc e n) = show e ++ "." ++ n
    show (Lub a b) = "(" ++ show a ++ " \\/ " ++ show b ++ ")"

instance Show Value where
    show = go 0
        where
        go indent val | null inner = "{}"
                      | otherwise = "{\n" ++ unlines inner ++ replicate indent ' ' ++ "}"
            where
            inner = [ replicate indent' ' ' ++ x ++ " = " ++ go indent' y | (x,y) <- Map.assocs r ]
            indent' = indent+2
            r = fix val
