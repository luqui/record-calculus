module LP.Parser where

import qualified Text.Parsec as P
import qualified Data.Char as Char
import LP.AST
import Control.Applicative

type Parser = P.Parsec String ()

tok :: String -> Parser String
tok = tokP . P.string

tokP :: Parser a -> Parser a
tokP = (<* P.spaces)

var :: Parser AST
var = Var <$> ident

ident :: Parser Name
ident = tokP . P.many1 . P.satisfy $ \c -> not (c `elem` " \t\n\\.()")

lambda :: Parser AST
lambda = flip (foldr Lambda) <$> (tok "\\" *> P.many1 ident) <*> (tok "." *> expr)

parenExpr :: Parser AST
parenExpr = tok "(" *> expr <* tok ")"

expr :: Parser AST
expr = foldl1 App <$> P.many1 (P.choice [parenExpr, lambda, var])

input :: Parser AST
input = expr <* P.eof

parse :: String -> AST
parse s = case P.parse input "input" s of
    Left err -> error (show err)
    Right ast -> ast
