{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lib ( startApp, app ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8      as I
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Either
import qualified Data.HashMap.Strict        as HM
import           Data.Map                   as Map
import           Data.Maybe
import           Data.String
import           Data.Text                  as T (Text, unpack)
import           Debug.Trace
import           GHC.Generics
import           MixedExpressions
import           MixedParser
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Safe                       as X (headMay, headNote, initMay,
                                                  tailMay)
import           Servant
import           Text.Parsec
import           UrlEncodeHelpers
import           Web.FormUrlEncoded

data ZingTreeVars a = ZingTreeVars{
  message   :: Maybe a,
  sessionId :: Maybe Text,
  agentName :: Maybe Text,
  vars      :: Map Text [Text]
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

boolToString (BoolVal True)  = StringVal "True"
boolToString (BoolVal False) = StringVal "False"
boolToString v               = v

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
