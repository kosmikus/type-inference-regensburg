module Parser where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec as P
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

import Syntax

tok :: TokenParser a
tok = haskell

ident :: Parser String
ident = identifier tok

sym :: String -> Parser String
sym = symbol tok

keyw :: String -> Parser ()
keyw = reserved tok

int :: Parser Integer
int = natural tok

expr :: Parser Expr
expr = foldl1 App <$> many1 term

term :: Parser Expr
term =
      Var <$> ident
  -- <|> Num <$> int
  <|> lam <$ (sym "\\" <?> "lambda") <*> many1 ident <* sym "->" <*> expr
  <|> letfun <$ keyw "let" <*> ident <*> many ident <* sym "=" <*> expr
             <* keyw "in"  <*> expr
  <|> parens tok expr

parseExpr :: String -> Either ParseError Expr
parseExpr xs = parse (whiteSpace tok *> expr <* eof) "interactive" xs

decl :: Parser (Name, Expr)
decl = (\ x xs e -> (x, lam xs e)) <$ keyw "let" <*> ident <*> many ident <* sym "=" <*> expr

toplevel :: Parser (Name, Expr)
toplevel = ((,) "it") <$> try expr <|> decl

parseToplevel :: String -> Either ParseError (Name, Expr)
parseToplevel = parse (whiteSpace tok *> toplevel <* eof) "interactive"
