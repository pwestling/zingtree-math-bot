{-# LANGUAGE TupleSections #-}

module MathExpressions where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Safe as S
import Text.Read
import Data.Maybe

newtype Var = Var T.Text deriving Show
newtype Val = Val Double deriving Show
data BinaryOp = Plus | Minus | Mult | Divide deriving Show
data UnaryOp = Negate deriving Show
data SymbolicExpression =
  SBinaryExpression BinaryOp SymbolicExpression SymbolicExpression |
  SUnaryExpression UnaryOp SymbolicExpression |
  VarExpression Var |
  SValExpression Val
  deriving Show

data ConcreteExpression =
  CBinaryExpression BinaryOp ConcreteExpression ConcreteExpression |
  CUnaryExpression UnaryOp ConcreteExpression |
  CValExpression Val
  deriving Show

data Equality = Equality Var SymbolicExpression deriving Show
newtype Equalities = Equalities [Equality] deriving Show

runMath :: M.Map T.Text [T.Text] -> Equalities -> Maybe (M.Map T.Text Double)
runMath varMap eqs = M.map runExpression <$> makeConcrete varMap eqs

makeConcrete :: M.Map T.Text [T.Text] -> Equalities -> Maybe (M.Map T.Text ConcreteExpression)
makeConcrete vars (Equalities eqs) = result where
  concretes = fmap (makeEqConcrete vars) eqs
  result = if any isNothing concretes then Nothing else Just $ M.fromList $ map fromJust concretes

makeEqConcrete :: M.Map T.Text [T.Text] -> Equality -> Maybe (T.Text, ConcreteExpression)
makeEqConcrete vars (Equality (Var name) expr) = (name,) <$> makeExpConcrete vars expr

makeExpConcrete :: M.Map T.Text [T.Text] -> SymbolicExpression -> Maybe ConcreteExpression
makeExpConcrete vars (SBinaryExpression op s1 s2) = CBinaryExpression op <$> makeExpConcrete vars s1 <*> makeExpConcrete vars s2
makeExpConcrete vars (SUnaryExpression op s) = CUnaryExpression op <$> makeExpConcrete vars s
makeExpConcrete vars (SValExpression val) = Just $ CValExpression val
makeExpConcrete vars (VarExpression (Var var)) = result where
  val = S.headMay =<< M.lookup var vars
  numVal = readMaybe =<< T.unpack <$> val
  result = CValExpression . Val <$> numVal

runExpression :: ConcreteExpression -> Double
runExpression (CBinaryExpression op s1 s2) = runBinOp op (runExpression s1) (runExpression s2)
runExpression (CUnaryExpression op s) = runUnOp op (runExpression s)
runExpression (CValExpression (Val v)) = v

runUnOp :: UnaryOp -> Double -> Double
runUnOp Negate d = -d

runBinOp :: BinaryOp -> Double -> Double -> Double
runBinOp Plus s1 s2 = s1 + s2
runBinOp Minus s1 s2 = s1 - s2
runBinOp Mult s1 s2 = s1 * s2
runBinOp Divide s1 s2 = s1 / s2
