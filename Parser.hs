module Parser (readTerm) where

import Syntax

import Control.Applicative
import Control.Monad

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim (parse, try)
import Text.Parsec.String

type Ctx = [String]

parseIdent :: Parser String
parseIdent = (:) <$> letter <*> many alphaNum

parseId :: Ctx -> Parser Term
parseId ctx = do
  id <- parseIdent
  return $ case id of
    "cc" -> CC
    s    -> Idx (findId s ctx)

parseLambda :: Ctx -> Parser Term
parseLambda ctx = do
  char '/'
  id   <- parseIdent
  char '.'
  term <- parseTerm (id:ctx)
  return (Abs term)

parseApp :: Ctx -> Parser Term
parseApp ctx = do
  let parseTerm' = try (addParen (parseApp ctx)) <|> try (addParen (parseLambda ctx)) <|> parseId ctx
  t1 <- parseTerm'
  terms <- some (some space *> parseTerm')
  return $ foldl App t1 terms

addParen :: Parser a -> Parser a
addParen p = char '(' *> p <* char ')'

parseTerm :: Ctx -> Parser Term
parseTerm ctx =
      try (parseApp ctx)
  <|> try (parseLambda ctx)
  <|> parseId ctx
  <|> addParen (parseTerm ctx)
  
readTerm s = case (parse (parseTerm [] <* eof) "" s) of
  Left err -> error (show err)
  Right t  -> t

findId s ctx =
  let f _ [] = error $ s ++ " not found in context."
      f n (s':ss)
        | s == s'   = n
        | otherwise = f (n+1) ss
  in f 0 ctx
