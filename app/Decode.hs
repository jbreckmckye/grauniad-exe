{-# LANGUAGE OverloadedStrings #-}

module Decode (
  Front
) where

import Data.Text

-- Fronts are the lists of links that send you to articles
data Front
  = Front { frName :: Text, frHeads :: [ArticleHead] }


-- e.g. POLITICS: Tommy Wiseau crowned Life Emperor of the Universe
data ArticleHead
  = ArticleHead { ahTopic :: Text, ahName :: Text }

instance Show ArticleHead where
  show (ArticleHead topic name) = show $ (toUpper topic) <> ": " <> name



-- we'll need to add tagSoup to this