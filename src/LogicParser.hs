{-# LANGUAGE FlexibleInstances #-}

module LogicParser where

import           Data.String
import           ExpressionParser
import           LogicExpressions
import           Text.Parsec

instance IsString (Either ParseError LogicEqualities) where
  fromString = runParser (equalitiesParser logicParseTable) () ""

type LogicParser a = SEParser LogicBinOp LogicUnOp Bool a

logicParseTable = ParserTable {
  valParser = val,
  unaryOpParsers = op ["not", "~"] Not,
  binaryOpParsersByPrecedence = [op orNames Or, op andNames And]}

ifMatches :: Parsec s m a -> b -> Parsec s m b
ifMatches parser val = parser *> return val

val :: LogicParser Bool
val = try (ifMatches parseTrue True) <|> ifMatches parseFalse False where
  parseTrue = choice $ map string ["True", "TRUE", "yes"]
  parseFalse = choice $ map string ["False", "FALSE", "no"]

andNames = ["and", "AND", "And", "&&", "&", "^"]
orNames = ["or", "OR", "Or", "||", "|", "V", "v"]

op :: [String] -> a -> LogicParser a
op opNames op = do
  opChar <- choice $ map string opNames
  spaces
  return op
