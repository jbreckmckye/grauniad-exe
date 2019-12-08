{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Client

import Context ( apiKey )

main :: IO ()
main = do
    key <- apiKey
    putStrLn $ "API key is " ++ key
