module LogicExpressions where

import           Expressions
import           Text.Read


data LogicBinOp = And | Or deriving Show
data LogicUnOp = Not deriving Show

type LogicSExp = SymbolicExpression LogicBinOp LogicUnOp Bool
type LogicCExp = ConcreteExpression LogicBinOp LogicUnOp Bool
type LogicEqualities = Equalities LogicBinOp LogicUnOp Bool

logicLang = ExpressionLang {
  binaryEvaluator = runLBinOp,
  unaryEvaluator = runLUnOp,
  readVal = readMaybe
}

runLogic = runExpressions logicLang

runLUnOp :: LogicUnOp -> Bool -> Bool
runLUnOp Not = not

runLBinOp :: LogicBinOp -> Bool -> Bool -> Bool
runLBinOp And = (&&)
runLBinOp Or  = (||)
