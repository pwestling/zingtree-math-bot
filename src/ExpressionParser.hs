module ExpressionParser where

import           Data.Either
import           Data.String
import qualified Data.Text   as T
import           Expressions
import           Text.Parsec

type SEParser b u v a = Parsec String () a

equalitiesParser :: ParserTable b u v -> SEParser b u v (Equalities b u v)
equalitiesParser tab = Equalities <$> equality tab `sepBy1` char ';'

data ParserTable b u v = ParserTable {
  valParser                   :: Parsec String () v,
  unaryOpParsers              :: Parsec String () u,
  binaryOpParsersByPrecedence :: [Parsec String () b]
}

equality :: ParserTable b u v -> SEParser b u v (Equality b u v)
equality tab = do
  spaces
  rhs <- var
  spaces
  char '='
  spaces
  expr <- expression tab
  spaces
  return (Equality rhs expr)

validName = many1 (letter <|> digit <|> oneOf "_-")

var :: SEParser b u v Var
var = Var . T.pack <$> validName

varExp = VarExpression <$> (var <* spaces)

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

expression :: ParserTable b u v -> SEParser b u v (SymbolicExpression b u v)
expression tab = do
  let binarys = map expressionWithPrecedence (binaryOpParsersByPrecedence tab)
  let allBinOps = foldl (.) id binarys
  allBinOps $ isoExp tab

expressionWithPrecedence ::
  SEParser b u v b ->
  SEParser b u v (SymbolicExpression b u v) ->
  SEParser b u v (SymbolicExpression b u v)
expressionWithPrecedence binaryOp lowerPrecedences  = chainl1 lowerPrecedences (asOp binaryOp)

asOp :: SEParser b u v b -> SEParser b u v (SymbolicExpression b u v -> SymbolicExpression b u v -> SymbolicExpression b u v)
asOp b = SBinaryExpression <$> b

isoExp :: ParserTable b u v -> SEParser b u v (SymbolicExpression b u v)
isoExp tab = do
  let valExp = SValExpression <$> (valParser tab <* spaces)
  try (parenExp tab) <|> try (unExp tab) <|> try valExp <|> varExp

parenExp :: ParserTable b u v -> SEParser b u v (SymbolicExpression b u v)
parenExp tab = do
  char '('
  spaces
  e1 <- expression tab
  spaces
  char ')'
  spaces
  return e1

unExp :: ParserTable b u v -> SEParser b u v (SymbolicExpression b u v)
unExp tab = do
  op <- unaryOpParsers tab
  spaces
  e1 <- isoExp tab
  spaces
  return (SUnaryExpression op e1)
