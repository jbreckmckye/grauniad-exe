{-# LANGUAGE OverloadedStrings #-}

module Decode (
  frontsFromHTML
) where

import qualified Data.Text as T
import Data.Text ( Text )
import Text.HTML.TagSoup


-- Types
-- ----------------------------------------------------------------------

data Front
  = Front { frName :: Text, frHeads :: [ArticleHead] }

instance Show Front where
  show (Front name heads) = show $ "Section: " <> (show name) <> "; articles: " <> (show heads)


data ArticleHead
  = ArticleHead { ahTopic :: Text, ahName :: Text }

instance Show ArticleHead where
  show (ArticleHead topic name) = show $ (T.toUpper topic) <> "/" <> name


-- Operations
-- ----------------------------------------------------------------------

frontsTags :: [Tag Text] -> [[Tag Text]]
frontsTags tags = partitions predicate tags
  where predicate = (~== ("<section>" :: String))


frontName :: [Tag Text] -> Text
frontName tags = extractId firstTag
  where extractId = fromAttrib "id"
        firstTag = head tags


frontHeads :: [Tag Text] -> [ArticleHead]
frontHeads tags = map parseHead links
  where links = getLinkContents tags


fronts :: [Tag Text] -> [Front]
fronts tags = map construct tagSets
   where tagSets = frontsTags tags
         construct tagSet = Front (frontName tagSet) (frontHeads tagSet)


frontsFromHTML :: Text -> [Front]
frontsFromHTML html = fronts $ parseTags html



-- -- Get headline section and children

-- headlineSec :: Text -> [Tag Text]
-- headlineSec html = headlineSec' . parseTags $ html

-- headlineSec' :: [Tag Text] -> [Tag Text]
-- headlineSec' tags = cutAfter . cutBefore $ tags
--   where cutBefore = dropWhile (~/= ("<section id='headlines'>" :: String))
--         cutAfter = takeWhile (~/= ("</section>" :: String))


-- -- Get articles from a section

-- getHeads :: Text -> [ArticleHead]
-- getHeads html = map parseHead (getLinkContents . headlineSec $ html)


getLinkContents :: [Tag Text] -> [[Tag Text]]
getLinkContents [] = []
getLinkContents tags = (firstLink : rest)
  where firstLink = takeWhile (~/= ("</a>" :: String)) afterOpen
        afterOpen = dropWhile (~/= ("<a class='fc-item__link'>" :: String)) tags
        
        rest      = getLinkContents restTags
        restTags  = drop (length firstLink) afterOpen


parseHead :: [Tag Text] -> ArticleHead
parseHead tags = ArticleHead topic name
  where topic = T.strip . innerText $ firstSpan
        name  = T.strip . innerText $ restTags

        firstSpan = takeWhile (~/= ("</span>" :: String)) tags
        restTags = drop (length firstSpan) tags



-- getLinks :: Text -> [Text]
-- getLinks html = (getLinks' . parseTags) html


-- getLinks' :: [Tag Text] -> [Text]
-- getLinks' [] = []
-- getLinks' xs = (firstLink : others)
--   where afterOpen = dropWhile (~/= ("<a class='fc-item__link'>" :: String)) $ xs
--         beforeClose = takeWhile (~/= ("</a>" :: String)) $ afterOpen
--         firstLinkLen = length beforeClose
--         firstLink = parseLink beforeClose
--         others = getLinks' $ (drop firstLinkLen afterOpen)


-- parseLink :: [Tag Text] -> Text
-- parseLink tags = (formatKicker kicker) <> headline
--   where kicker = innerText . takeWhile (~/= ("</span>" :: String)) $ tags
--         headline = innerText . dropWhile (~/= ("<span class='js-headline-text'>" :: String)) $ tags

--         formatKicker "" = ""
--         formatKicker text = (T.toUpper text) <> ": "
