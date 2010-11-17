module RC.Parser where

import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified Data.Map as Map
import RC.AST
import Control.Applicative
import Data.Functor.Identity

type Parser = P.ParsecT String () Identity

tok :: String -> Parser String
tok = tokP . P.string

tokP :: Parser a -> Parser a
tokP = (<* whitespace)

node :: (P.SourcePos -> a) -> Parser a
node f = f <$> P.getPosition

whitespace :: Parser ()
whitespace = () <$ P.many (spaces <|> comment)
    where
    comment = P.string "--" *> P.many (P.satisfy (/= '\n')) *> P.string "\n"
    spaces = P.many1 (P.satisfy Char.isSpace)

ident :: Parser Name
ident = tokP . P.many1 . P.satisfy $ \c -> not (c `elem` " \t\n\\.(){}=\\/")

var :: Parser AST
var = node Var <*> ident

record :: Parser AST
record = node Record <*> (Map.fromList <$> (tok "{" *> many binding) <* tok "}")

binding :: Parser (Name, AST)
binding = do
    x <- ident
    pos <- P.getPosition
    y <- P.option (Record pos Map.empty) (tok "=" *> expr)
    return (x,y)

accessor :: Parser (AST -> AST)
accessor = node (\p x y -> Acc p y x) <*> (tok "." *> ident)

smallExpr :: Parser AST
smallExpr = foldl (flip id) <$> term <*> many accessor
    where
    term = var <|> record <|> parenExpr

expr :: Parser AST
expr = foldl (\c (p,x) -> Lub p c x) <$> smallExpr <*> P.many (tok "\\/" *> node (,) <*> smallExpr)

parenExpr :: Parser AST
parenExpr = tok "(" *> expr <* tok ")"

complete :: Parser a -> Parser a
complete p = p <* P.eof
