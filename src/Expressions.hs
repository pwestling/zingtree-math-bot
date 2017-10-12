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
import Safe

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
runExpressions lang varMap (Equalities eqs) = makeConcrete parsedVarValues computer M.empty eqs where
  computer = makeComputer (binaryEvaluator lang) (unaryEvaluator lang)
  parsedVarValues = parseVars (readVal lang) varMap

makeComputer :: (b -> v -> v -> v) -> (u -> v -> v) -> ConcreteExpression b u v -> v
makeComputer b u (CBinaryExpression op s1 s2) = b op (makeComputer b u  s1) (makeComputer b u s2)
makeComputer b u (CUnaryExpression op s) = u op (makeComputer b u s)
makeComputer b u  (CValExpression v) = v

makeConcrete :: M.Map T.Text (Maybe v) -> (ConcreteExpression b u v -> v) -> M.Map T.Text v ->
  [Equality b u v] -> Maybe (M.Map T.Text v)
makeConcrete vars computer results [e] = do
    (key,expr,calcVal, newResults) <- makeConcrete' vars computer results e
    return newResults
makeConcrete vars computer results (e1 : es) = do
    (key,expr,calcVal, newResults) <- makeConcrete' vars computer results e1
    let newVars = M.insert key (Just calcVal) vars
    makeConcrete newVars computer newResults es

makeConcrete' vars computer results e = do
  let concrete = makeEqConcrete vars e
  (key, expr) <- concrete
  let calcVal = computer expr
  let newResults = M.insert key calcVal results
  return (key,expr,calcVal, newResults)

makeEqConcrete :: M.Map T.Text (Maybe v) -> Equality b u v -> Maybe (T.Text, ConcreteExpression b u v)
makeEqConcrete vars (Equality (Var name) expr) = (name,) <$> makeExpConcrete vars expr

makeExpConcrete :: M.Map T.Text (Maybe v) -> SymbolicExpression b u v -> Maybe (ConcreteExpression b u v)
makeExpConcrete vars (SBinaryExpression op s1 s2) = CBinaryExpression op <$> makeExpConcrete vars s1 <*> makeExpConcrete vars s2
makeExpConcrete vars (SUnaryExpression op s) = CUnaryExpression op <$> makeExpConcrete vars s
makeExpConcrete vars (SValExpression val) = Just $ CValExpression val
makeExpConcrete vars (VarExpression (Var var)) = result where
  val = join $ M.lookup var vars
  result = CValExpression <$> val

readEitherVal :: (String -> Maybe m) -> (String -> Maybe m) -> String -> Maybe m
readEitherVal fa fb s = result where
  l = fa s
  r = fb s
  result = headMay $ catMaybes [l, r]

data ExpressionLang b u v = ExpressionLang {
  binaryEvaluator :: b -> v -> v -> v,
  unaryEvaluator :: u -> v -> v,
  readVal :: String -> Maybe v
}
