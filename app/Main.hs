{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString.Char8 as B

-- import Context ( apiKey )

data Args
    = SectionArgs { section :: String, ref :: Int }
    | UrlArgs     { urlSlug :: String }
    | NoArgs

instance Show Args where
    show (SectionArgs section _) = "Display article from section " ++ section
    show (UrlArgs urlSlug)       = "Display article from URL " ++ urlSlug
    show (NoArgs)                = "Display a front"

main :: IO ()
main = do
    args <- parseArgs
    putStrLn $ "Args is " ++ show args
    -- case args of
    --     SectionArgs section ref -> showArticle (fromSection section ref)
    --     UrlArgs urlSlug         -> showArticle (matchUrl urlSlug)
    --     NoArgs                  -> showFront

parseArgs :: IO Args
parseArgs = do
    rawArgs <- getArgs
    return $ case (length rawArgs) of
        0       -> NoArgs
        1       -> UrlArgs (rawArgs !! 0)
        2       -> SectionArgs (rawArgs !! 0) (parseInt $ rawArgs!!1)

parseInt :: String -> Int
parseInt text = read text :: Int

-- parseArgs :: IO Args
-- parseArgs = do
--     args <- getArgs
--     case args of
--         section ref -> SectionArgs section (read ref :: Int)
--         urlSlug     -> UrlArgs urlSlug
        


-- main :: IO ()
-- main = do
--     key <- apiKey
--     putStrLn $ "API key is " ++ key
--     txt <- fetchText
--     -- L8.putStrLn txt
--     putStrLn $ "resp length is " ++ (show $ length $ L8.unpack txt)
--     mapM_ putStrLn $ header ("API key is " ++ key) 80

-- fetchText :: IO L8.ByteString
-- fetchText = do
--     response <- httpLBS "https://www.theguardian.com/uk"

--     putStrLn $ "The status code was: " ++
--                show (getResponseStatusCode response)
--     print $ getResponseHeader "Content-Type" response
--     return $ getResponseBody response


-- header :: String -> Int -> [String]
-- header text width = [top, body, tail]
--     where top  = wrapped '+' $ printMany '-' innerWidth
--           body = wrapped '|' $ padded text innerWidth
--           tail = top
--           innerWidth = width - 2

-- printMany :: Char -> Int -> String
-- printMany c i = printMany' c i ""

-- printMany' :: Char -> Int -> String -> String
-- printMany' c 0 done = done
-- printMany' c n done = printMany' c nextN (done ++ [c])
--     where nextN = n - 1

-- padded :: String -> Int -> String
-- padded text width = text ++ (printMany ' ' charsLeft)
--     where charsLeft = width - (length text)

-- wrapped :: Char -> String -> String
-- wrapped outer inner = [outer] ++ " " ++ inner ++ " " ++ [outer]