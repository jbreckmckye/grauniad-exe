module Cmd (
  Cmd
) where

import System.Environment

data Cmd
  = CmdHeadlines
  | CmdSectionArticle   { csaName :: String, csaKey :: Integer }
  | CmdUrlArticle       { cuaUrl :: String }
  | CmdUnknown

-- Get user command
cmdFromArgs :: IO Cmd
cmdFromArgs = do
  args <- getArgs
  return $ extract args


-- Extract command from args
extract :: [String] -> Cmd
extract args = case args of
  [ ]                 -> CmdHeadlines
  [url]               -> CmdUrlArticle url
  [section, key]      -> CmdSectionArticle section ( read key )
  unknown             -> CmdUnknown