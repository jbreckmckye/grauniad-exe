module Fetch (
  fetchHtml
) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B
import Data.Text
import Data.Text.Encoding


fetchHtml :: IO Text
fetchHtml = do
  req <- parseRequest "https://www.theguardian.com/uk"
  res <- httpBS req
  let body = getResponseBody res
  return $ decodeUtf8 body
