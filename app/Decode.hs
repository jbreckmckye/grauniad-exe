{-# LANGUAGE OverloadedStrings #-}

module Decode (
  Front,
  getTitle,
  getLinks
) where

import qualified Data.Text as T
import Data.Text ( Text )
import Text.HTML.TagSoup
-- import Prelude hiding ( take, words, drop, unwords )
import Fetch


-- Types
-- ----------------------------------------------------------------------

data Front
  = Front { frName :: Text, frHeads :: [ArticleHead] }

data ArticleHead
  = ArticleHead { ahTopic :: Text, ahName :: Text }

instance Show ArticleHead where
  show (ArticleHead topic name) = show $ (T.toUpper topic) <> ": " <> name


-- Operations
-- ----------------------------------------------------------------------


getTitle :: Text -> IO Text
getTitle html = do
  let tags = parseTags html
  return $ getTitle' tags


getTitle' :: [Tag Text] -> Text
getTitle' = innerText . take 2 . dropWhile (~/= ("<h1>" :: String))


getLinks :: Text -> [Text]
getLinks html = (getLinks' . parseTags) html


getLinks' :: [Tag Text] -> [Text]
getLinks' [] = []
getLinks' xs = (firstLink : others)
  where afterOpen = dropWhile (~/= ("<a class='fc-item__link'>" :: String)) $ xs
        beforeClose = takeWhile (~/= ("</a>" :: String)) $ afterOpen
        firstLinkLen = length beforeClose
        firstLink = parseLink beforeClose
        others = getLinks' $ (drop firstLinkLen afterOpen)


parseLink :: [Tag Text] -> Text
parseLink tags = innerText tags

-- getTitle :: Text -> IO Text
-- getTitle html = do
--   let tags = parseTags html
--   let content = fromTagText (parse tags !! 1)
--   return content

--   where parse = innerText . take 1 . dropWhile (~/= "<h1>")


-- getTitle' :: Text -> Text
-- getTitle' = innerText . take 1 . dropWhile (~/= "<h1>")

-- getTitle' :: Text -> Text
-- getTitle' = innerText . take 1 . dropWhile (~/= "<h1>")


-- openURL :: String -> IO String


-- currentTime :: IO ()
-- currentTime = do
--     tags <- parseTags <$> openURL "http://www.timeanddate.com/worldclock/uk/london"
--     let time = fromTagText (dropWhile (~/= "<span id=ct>") tags !! 1)
--     putStrLn time


-- openURL :: String -> IO String
-- openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- haskellLastModifiedDateTime :: IO ()
-- haskellLastModifiedDateTime = do
--     src <- openURL "http://wiki.haskell.org/Haskell"
--     let lastModifiedDateTime = fromFooter $ parseTags src
--     putStrLn $ "wiki.haskell.org was last modified on " ++ lastModifiedDateTime
--     where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= "<li id=lastmod>")

-- haskellLastModifiedDateTime :: IO ()
-- haskellLastModifiedDateTime = do
--     src <- fetchHtml
--     let res = parseTags (T.unpack src)
--     let lastModifiedDateTime = fromFooter res
--     putStrLn $ "wiki.haskell.org was last modified on " ++ lastModifiedDateTime
--     where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= ("<li id=lastmod>" :: String))