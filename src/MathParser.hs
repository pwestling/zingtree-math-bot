{-# LANGUAGE FlexibleInstances #-}

module MathParser where

import Text.Parsec
import MathExpressions
import qualified Data.Text as T
import Data.String
import Data.Either

instance IsString (Either ParseError Equalities) where
  fromString s = parse mathParser "" s

mathParser :: Parsec String () Equalities
mathParser = Equalities <$> equality `sepBy` char ';'

equality :: Parsec String () Equality
equality = do
  spaces
  rhs <- var
  spaces
  char '='
  spaces
  expr <- expression
  return (Equality rhs expr)

var :: Parsec String () Var
var = Var . T.pack <$> many1 (letter <|> digit <|> oneOf "_-")

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

val :: Parsec String () Val
val = Val . read <$> num <++> possibly (char '.' <:> num) where
  num = many1 digit
  possibly = option ""

expression :: Parsec String () SymbolicExpression
expression = try binExp <|> try unExp <|> isoExp

valExp = SValExpression <$> val
varExp = VarExpression <$> var

isoExp :: Parsec String () SymbolicExpression
isoExp = try parenExp <|> try valExp <|> varExp

binExp :: Parsec String () SymbolicExpression
binExp = do
  e1 <- isoExp
  spaces
  op <- binOp
  spaces
  e2 <- expression
  return (SBinaryExpression op e1 e2)

parenExp :: Parsec String () SymbolicExpression
parenExp = do
  char '('
  spaces
  e1 <- expression
  spaces
  char ')'
  return e1

unExp :: Parsec String () SymbolicExpression
unExp = do
  op <- unOp
  spaces
  e1 <- isoExp
  return (SUnaryExpression op e1)

binOp :: Parsec String () BinaryOp
binOp = do
  opChar <- oneOf "+-*/"
  return (toBinOp opChar)

toBinOp :: Char -> BinaryOp
toBinOp '+' = Plus
toBinOp '-' = Minus
toBinOp '*' = Mult
toBinOp '/' = Divide

unOp :: Parsec String () UnaryOp
unOp = do
  opChar <- char '-'
  return (toUnOp opChar)

toUnOp :: Char -> UnaryOp
toUnOp '-' = Negate
