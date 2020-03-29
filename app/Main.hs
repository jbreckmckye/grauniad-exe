{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Prelude hiding ( putStrLn )

import Cmd ( cmdFromArgs )
import Fetch ( fetchHtml )
import Decode ( frontsFromHTML )
import RenderText ( footer, h1, h2 )
import Data.Text.IO ( putStrLn )

main :: IO ()
main = do
  let heading = h1 "GRAUNIAD.EXE"
  mapM_ putStrLn heading

  let heading2 = h2 "This is the news!"
  mapM_ putStrLn heading2

  let heading3 = h2 "This is the news also! I am really long and may possibly wrap. Who knows what could happen in the crazy world of this program? Let's explore and find out..."
  mapM_ putStrLn heading3

  html <- fetchHtml
  let heads = frontsFromHTML html
  print heads

  let ftr = footer "Totally unofficial, toy client for <<The Guardian>>"
  mapM_ putStrLn ftr
