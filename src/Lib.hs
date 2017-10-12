{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( startApp, app ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text as T (Text, unpack)
import Control.Applicative
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant
import Web.FormUrlEncoded
import Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as I
import GHC.Generics
import Network.Wai
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.String
import Safe as X (headMay, headNote, initMay,tailMay)
import UrlEncodeHelpers
import MixedExpressions
import MixedParser
import Text.Parsec
import Data.Either

data ZingTreeVars a = ZingTreeVars{
  message :: Maybe a,
  sessionId :: Maybe Text,
  agentName :: Maybe Text,
  vars :: Map Text [Text]
} deriving (Show, Generic)

instance IsString a => FromForm (ZingTreeVars a) where
  fromForm form = traceShow form $ Right ZingTreeVars {message = message, sessionId = sess, agentName = agent, vars = vars} where
        sess = getSingleValParam "session_id" form
        agent = getSingleValParam "agent_name" form
        message = (fromString . T.unpack) <$> getSingleValParam "message" form
        vars = withoutKeys ["session_id", "agent_name", "message"] form

type MathAPI = "calculate" :>
  ReqBody '[FormUrlEncoded] (ZingTreeVars (Either ParseError MixedEqualities)) :>
  Post '[JSON] (Map T.Text Val)

type API = MathAPI

fromRight (Right a) = a
fromLeft (Left a) = a

boolToString (BoolVal True) = StringVal "True"
boolToString (BoolVal False) = StringVal "False"
boolToString v = v

calculateEndpoint :: ZingTreeVars (Either ParseError MixedEqualities) ->
  Either ParseError MixedEqualities ->
  Handler (Map T.Text Val)
calculateEndpoint ztv m = if isRight m then maybe varErr return (Map.map boolToString <$> runMixedExpressions (vars ztv) (fromRight m))
  else traceShow (fromLeft m) parseErr where
    parseErr = throwError err404 {errBody = "Malformed message expression"}
    varErr = throwError err404 {errBody = "Unrecognized variable name"}

runMessageEndpoint :: (ZingTreeVars a -> a -> Handler b) -> ZingTreeVars a -> Handler b
runMessageEndpoint f v = maybe err (f v) m where
  m = message v
  err = throwError err404 {errBody = "This endpoint requires a message"}

server :: Server API
server a = do
  result <- traceShow a (runMessageEndpoint calculateEndpoint a)
  let json = encode $ toJSON result
  return $ traceShow json result

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

startApp :: Int -> IO ()
startApp port = withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings app
