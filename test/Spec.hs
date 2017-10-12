{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Base
import Data.String

main :: IO ()
main = hspec spec

postFormUrl path = request methodPost path [(hContentType, "application/x-www-form-urlencoded")]

spec :: Spec
spec = with (return app) $
    describe "POST /calculate" $
        it "responds with 200" $ do
            let equation = urlEncode "result=(f > g) && 10 * 3 > 5 and k - 4 == 0; result2 = f - g * k"
            let postdata = fromString $ "message=" ++ equation ++ "&f=3&g=2&k=4"
            let result = "{\"result\":\"True\",\"result2\":-5}"
            postFormUrl "/calculate" postdata `shouldRespondWith` result
