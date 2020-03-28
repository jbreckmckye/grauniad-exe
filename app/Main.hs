{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Prelude hiding ( putStrLn )

import Cmd ( cmdFromArgs )
import Fetch ( fetchHtml )
import Decode ( frontsFromHTML )
import Render ( renderHeading )
import Data.Text.IO ( putStrLn )

main :: IO ()
main = do
  let heading = renderHeading "Grauniad.exe"
  mapM_ putStrLn heading

  html <- fetchHtml
  let heads = frontsFromHTML html
  print heads
