module Fetch (
  fetchHtml
) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B
import Data.Text
import Data.Text.Encoding

homepage = pack "https://www.theguardian.com/uk"


fetchHtml :: IO Text
fetchHtml = html homepage


html :: Text -> IO Text
html url = do
  req <- parseRequest (unpack url)
  res <- httpBS req
  let body = getResponseBody res
  return $ decodeUtf8 body
