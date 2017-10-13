{-# LANGUAGE FlexibleInstances #-}

module MixedParser where

import qualified ArithmeticParser as AP
import           Data.String
import           ExpressionParser
import qualified LogicParser      as LP
import           MixedExpressions as ME
import           Text.Parsec

type MixedParser a = SEParser BinOp UnOp Val a

instance IsString (Either ParseError MixedEqualities) where
  fromString = runParser (equalitiesParser mixedParseTable) () ""

stringDelim :: MixedParser String
stringDelim = choice [string "\\'", string "\\\""]

mixedParseTable = ParserTable {
  valParser = choice [NumVal <$> AP.val, BoolVal <$> LP.val, StringVal <$> (stringDelim *> validName <* stringDelim)],
  unaryOpParsers = choice [ArithUnOp <$> unaryOpParsers AP.arithmeticParseTable, LogicUnOp <$> unaryOpParsers LP.logicParseTable],
  binaryOpParsersByPrecedence =
    map (Logic <$>) (binaryOpParsersByPrecedence LP.logicParseTable) ++
    [compareOps [("<", ME.LT),(">", ME.GT), ("<=", ME.LTE), (">=", ME.GTE), ("==", ME.EQ), ("!=", ME.NE)]] ++
    map (Arith <$>) (binaryOpParsersByPrecedence AP.arithmeticParseTable)}

compareOps :: [(String,CompareBinOp)] -> MixedParser BinOp
compareOps ops =  result where
  toParser = \(s,b) -> string s *> return b <* spaces
  result = Compare <$> choice (map toParser ops)
