{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString.Char8 as B

import Context ( apiKey )

main :: IO ()
main = do
    key <- apiKey
    putStrLn $ "API key is " ++ key
    txt <- fetchText
    -- L8.putStrLn txt
    putStrLn $ "resp length is " ++ (show $ length $ L8.unpack txt)
    mapM_ putStrLn $ header ("API key is " ++ key) 80

fetchText :: IO L8.ByteString
fetchText = do
    response <- httpLBS "https://www.theguardian.com/uk"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    return $ getResponseBody response


header :: String -> Int -> [String]
header text width = [top, body, tail]
    where top  = wrapped '+' $ printMany '-' innerWidth
          body = wrapped '|' $ padded text innerWidth
          tail = top
          innerWidth = width - 2

printMany :: Char -> Int -> String
printMany c i = printMany' c i ""

printMany' :: Char -> Int -> String -> String
printMany' c 0 head = head
printMany' c n head = printMany' c nextN (head ++ [c])
    where nextN = n - 1

padded :: String -> Int -> String
padded text width = text ++ (printMany ' ' charsLeft)
    where charsLeft = width - (length text)

wrapped :: Char -> String -> String
wrapped outer inner = [outer] ++ " " ++ inner ++ " " ++ [outer]