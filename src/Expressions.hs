{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Expressions where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Safe as S
import Text.Read
import Data.Maybe
import Data.Semigroup
import Data.Either
import Control.Monad

newtype Var = Var T.Text deriving Show

data SymbolicExpression b u v =
  SBinaryExpression b (SymbolicExpression b u v) (SymbolicExpression b u v) |
  SUnaryExpression u (SymbolicExpression b u v) |
  VarExpression Var |
  SValExpression v
  deriving Show

data ConcreteExpression b u v =
  CBinaryExpression b (ConcreteExpression b u v) (ConcreteExpression b u v) |
  CUnaryExpression u (ConcreteExpression b u v) |
  CValExpression v
  deriving Show

data Equality b u v = Equality Var (SymbolicExpression b u v) deriving Show
newtype Equalities b u v = Equalities [Equality b u v] deriving Show

parseVars :: (String -> Maybe v) -> M.Map T.Text [T.Text] -> M.Map T.Text (Maybe v)
parseVars parser = M.map (\s -> parser =<< T.unpack <$> S.headMay s)

runExpressions ::
  ExpressionLang b u v ->
  M.Map T.Text [T.Text] ->
  Equalities b u v ->
  Maybe (M.Map T.Text v)
runExpressions lang varMap eqs = M.map computer <$> makeConcrete parsedVarValues eqs where
  computer = makeComputer (binaryEvaluator lang) (unaryEvaluator lang)
  parsedVarValues = parseVars (readVal lang) varMap

makeComputer :: (b -> v -> v -> v) -> (u -> v -> v) -> ConcreteExpression b u v -> v
makeComputer b u (CBinaryExpression op s1 s2) = b op (makeComputer b u  s1) (makeComputer b u s2)
makeComputer b u (CUnaryExpression op s) = u op (makeComputer b u s)
makeComputer b u  (CValExpression v) = v

makeConcrete :: M.Map T.Text (Maybe v) -> Equalities b u v -> Maybe (M.Map T.Text (ConcreteExpression b u v))
makeConcrete vars (Equalities eqs) = result where
  concretes = fmap (makeEqConcrete vars) eqs
  result = if any isNothing concretes then Nothing else Just $ M.fromList $ map fromJust concretes

makeEqConcrete :: M.Map T.Text (Maybe v) -> Equality b u v -> Maybe (T.Text, ConcreteExpression b u v)
makeEqConcrete vars (Equality (Var name) expr) = (name,) <$> makeExpConcrete vars expr

makeExpConcrete :: M.Map T.Text (Maybe v) -> SymbolicExpression b u v -> Maybe (ConcreteExpression b u v)
makeExpConcrete vars (SBinaryExpression op s1 s2) = CBinaryExpression op <$> makeExpConcrete vars s1 <*> makeExpConcrete vars s2
makeExpConcrete vars (SUnaryExpression op s) = CUnaryExpression op <$> makeExpConcrete vars s
makeExpConcrete vars (SValExpression val) = Just $ CValExpression val
makeExpConcrete vars (VarExpression (Var var)) = result where
  val = join $ M.lookup var vars
  result = CValExpression <$> val

data ExpressionLang b u v = ExpressionLang {
  binaryEvaluator :: b -> v -> v -> v,
  unaryEvaluator :: u -> v -> v,
  readVal :: String -> Maybe v
}
