module ArithmeticExpressions where

import Expressions
import Text.Read


data BinaryOp = Plus | Minus | Mult | Divide deriving Show
data UnaryOp = Negate deriving Show

type MathSExp = SymbolicExpression BinaryOp UnaryOp Double
type MathCExp = ConcreteExpression BinaryOp UnaryOp Double
type MathEqualities = Equalities BinaryOp UnaryOp Double

arithmeticLang = ExpressionLang {
  binaryEvaluator = runBinOp,
  unaryEvaluator = runUnOp,
  readVal = readMaybe
}

runArithmetic = runExpressions arithmeticLang

runUnOp :: UnaryOp -> Double -> Double
runUnOp Negate d = -d

runBinOp :: BinaryOp -> Double -> Double -> Double
runBinOp Plus s1 s2 = s1 + s2
runBinOp Minus s1 s2 = s1 - s2
runBinOp Mult s1 s2 = s1 * s2
runBinOp Divide s1 s2 = s1 / s2
