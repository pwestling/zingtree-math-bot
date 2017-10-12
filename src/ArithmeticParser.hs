{-# LANGUAGE FlexibleInstances #-}

module ArithmeticParser where

import ArithmeticExpressions
import ExpressionParser
import Text.Parsec
import Data.String

instance IsString (Either ParseError MathEqualities) where
  fromString = runParser (equalitiesParser arithmeticParseTable) () ""

type MathParser a = SEParser BinaryOp UnaryOp Double a

arithmeticParseTable = ParserTable {
  valParser = val,
  unaryOpParsers = unOp,
  binaryOpParsersByPrecedence = [binOp "+-", binOp "*/"]}

val :: MathParser Double
val = read <$> num <++> possibly (char '.' <:> num) where
  num = many1 digit
  possibly = option ""

binOp :: String -> MathParser BinaryOp
binOp ops = do
  opChar <- oneOf ops
  spaces
  return (toBinOp opChar)

toBinOp :: Char -> BinaryOp
toBinOp '+' = Plus
toBinOp '-' = Minus
toBinOp '*' = Mult
toBinOp '/' = Divide

unOp :: MathParser UnaryOp
unOp = do
  opChar <- char '-'
  spaces
  return (toUnOp opChar)

toUnOp :: Char -> UnaryOp
toUnOp '-' = Negate
