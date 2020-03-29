{-# LANGUAGE OverloadedStrings #-}

module RenderText (
  h1,
  h2
) where

import qualified Data.Text as T
import Data.Text ( Text )
import Data.List ( replicate )


width = 80
innerWidth = width - 2

h1 :: Text -> [Text]
h1 heading = [hline, middle, hline]
  where hline   = "+"  <> (repText "-" innerWidth)         <> "+"
        middle  = "| " <> heading <> (repText " " padding) <> "|"
        padding = innerWidth - 1 - (T.length heading)


h2 :: Text -> [Text]
h2 heading = txtLines ++ [underline]
  where txtLines = map (pad " ") wrappedLines
        wrappedLines = wrap innerWidth heading
        underline = " " <> repText "-" lineWidth
        lineWidth = min innerWidth (T.length heading)


pad :: Text -> Text -> Text
pad padChar input = padChar <> input <> padChar

repText :: Text -> Int -> Text
repText txt count = T.concat $ replicate count txt

wrap :: Int -> Text -> [Text]
wrap len txt = wrap' len ([], txt)

wrap' :: Int -> ([Text], Text) -> [Text]
wrap' len (prev, next)
  | (T.length next <= len) = prev ++ [next]
  | otherwise = wrap' len (
    (prev ++ [T.take len next], (T.drop len next))
  )

