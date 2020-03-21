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
  show (Front name heads) = show $ "Section: " <> (T.unpack name) <> "; articles: " <> (show heads)


data ArticleHead
  = ArticleHead { ahTopic :: Text, ahName :: Text }

instance Show ArticleHead where
  show (ArticleHead topic name) = T.unpack $ (T.toUpper topic) <> "/" <> name


-- Operations
-- ----------------------------------------------------------------------

frontsFromHTML :: Text -> [Front]
frontsFromHTML html = fronts $ parseTags html


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
fronts tags = cleanFronts parsed
  where parsed = map construct tagSets
        tagSets = frontsTags tags
        construct tagSet = Front (frontName tagSet) (frontHeads tagSet)


cleanFronts :: [Front] -> [Front]
cleanFronts = filter notEmpty
  where notEmpty front = length (name front) > 0
        name front = T.unpack $ frName front
  

-- need a cleanHeads fn too


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

