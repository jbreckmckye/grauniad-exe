{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Cmd ( cmdFromArgs )
import Fetch ( fetchHtml )
import Decode ( frontsFromHTML )

main :: IO ()
main = do
  cmd <- cmdFromArgs
  print (show cmd)
  html <- fetchHtml
  let heads = frontsFromHTML html
  print heads
