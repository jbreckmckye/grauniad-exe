module Fetch (
  fetchHtml
) where


import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B
import Data.Text

fetchHtml :: IO L8.ByteString
fetchHtml = do
  req <- parseRequest "https://www.theguardian.com/uk"
  response <- httpLBS req
  return $ getResponseBody response
