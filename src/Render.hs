{-# LANGUAGE OverloadedStrings #-}

module Render (
  renderHeading
) where

import qualified Data.Text as T
import Data.Text ( Text )
import Data.List ( replicate )


width = 80

renderHeading :: Text -> [Text]
renderHeading heading = [hline, middle, hline]
  where hline   ="+"   <> (repText "-" insideWidth)        <> "+"
        middle  = "| " <> heading <> (repText " " padding) <> "|"
        padding = insideWidth - 1 - (T.length heading)

        insideWidth = width - 2


repText :: Text -> Int -> Text
repText txt count = T.concat $ replicate count txt
