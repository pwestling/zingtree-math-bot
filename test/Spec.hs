{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8
import           Data.String
import           Lib                        (app)
import           Network.HTTP.Base
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

postFormUrl path = request methodPost path [(hContentType, "application/x-www-form-urlencoded")]

spec :: Spec
spec = with (return app) $
    describe "POST /calculate" $
        it "responds with 200" $ do
            let equation = urlEncode "result=(f > g) && 10 * 3 > 5 and k - 4 == 0; z=k; result2 = f - g * z"
            let postdata = fromString $ "message=" ++ equation ++ "&f=3&g=2&k=4"
            let result =  "{\"result\":\"True\",\"result2\":-5,\"z\":4}"
            postFormUrl "/calculate" postdata `shouldRespondWith` result
