module MixedExpressions where

import           ArithmeticExpressions
import           Data.Aeson
import           Data.Maybe
import           Debug.Trace
import           Expressions
import           LogicExpressions
import           Safe

data BinOp = Compare CompareBinOp | Arith BinaryOp | Logic LogicBinOp deriving Show
data CompareBinOp = GT | LT | EQ | GTE | LTE | NE deriving Show
data UnOp = ArithUnOp UnaryOp | LogicUnOp LogicUnOp deriving Show
data Val = BoolVal Bool | NumVal Double | StringVal String deriving Show

type MixedSExp = SymbolicExpression BinOp UnOp Val
type MixedCExp = ConcreteExpression BinOp UnOp Val
type MixedEqualities = Equalities BinOp UnOp Val

mixedExpressionLang = ExpressionLang {
  binaryEvaluator = runMixedBinOp,
  unaryEvaluator = runMixedUnOp,
  readVal = readMixedVal
}

readBoolOrNum :: String -> Maybe Val
readBoolOrNum = readEitherVal (fmap BoolVal . readMay) (fmap NumVal . readMay)

readMixedVal :: String -> Maybe Val
readMixedVal = readEitherVal readBoolOrNum (Just . StringVal)

instance ToJSON Val where
  toJSON (NumVal d)    = toJSON d
  toJSON (BoolVal b)   = toJSON b
  toJSON (StringVal s) = toJSON s

runMixedExpressions = runExpressions mixedExpressionLang

runMixedUnOp :: UnOp -> Val -> Val
runMixedUnOp (LogicUnOp o) (BoolVal v) = BoolVal $ runLUnOp o v
runMixedUnOp (ArithUnOp o) (NumVal v)  = NumVal $ runUnOp o v

runMixedBinOp :: BinOp -> Val -> Val -> Val
runMixedBinOp (Logic op) (BoolVal v1) (BoolVal v2) = BoolVal $ runLBinOp op v1 v2
runMixedBinOp (Arith op) (NumVal v1) (NumVal v2) = NumVal $ runBinOp op v1 v2
runMixedBinOp (Compare op) v1 v2 = BoolVal $ runCompareOp op v1 v2
runMixedBinOp a b c = traceShow (a,b,c) undefined

runCompareOp :: CompareBinOp -> Val -> Val -> Bool
runCompareOp MixedExpressions.GT (NumVal v1) (NumVal v2)       = v1 > v2
runCompareOp MixedExpressions.LT (NumVal v1) (NumVal v2)       = v1 < v2
runCompareOp MixedExpressions.GTE (NumVal v1) (NumVal v2)      = v1 >= v2
runCompareOp MixedExpressions.LTE (NumVal v1) (NumVal v2)      = v1 <= v2
runCompareOp MixedExpressions.EQ (NumVal v1) (NumVal v2)       = v1 == v2
runCompareOp MixedExpressions.EQ (BoolVal v1) (BoolVal v2)     = v1 == v2
runCompareOp MixedExpressions.NE (NumVal v1) (NumVal v2)       = v1 /= v2
runCompareOp MixedExpressions.NE (BoolVal v1) (BoolVal v2)     = v1 /= v2
runCompareOp MixedExpressions.EQ (StringVal v1) (StringVal v2) = v1 == v2
runCompareOp MixedExpressions.NE (StringVal v1) (StringVal v2) = v1 /= v2
