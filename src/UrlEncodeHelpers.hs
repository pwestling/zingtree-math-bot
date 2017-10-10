module UrlEncodeHelpers where
import qualified Data.HashMap.Strict as HM
import Web.FormUrlEncoded
import qualified Data.Text as T
import Safe
import qualified Data.Map as M

getSingleValParam :: T.Text -> Form -> Maybe T.Text
getSingleValParam key form = result where
   hashmap = unForm form
   result = HM.lookup key hashmap >>= headMay

withoutKeys :: [T.Text] -> Form -> M.Map T.Text [T.Text]
withoutKeys keys form = result where
  keyRemoveFn = foldl (.) id $ fmap HM.delete keys
  cleanedHashmap = keyRemoveFn (unForm form)
  result = M.fromList . HM.toList $ cleanedHashmap
