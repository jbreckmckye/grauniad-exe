module Cmd (
  Cmd,
  cmdFromArgs
) where

import System.Environment

data Cmd
  = CmdHeadlines
  | CmdSectionArticle   { csaName :: String, csaKey :: Integer }
  | CmdUrlArticle       { cuaUrl :: String }
  | CmdUnknown

instance Show Cmd where
  show (CmdHeadlines)                     = "Request for the latest headlines"
  show (CmdSectionArticle csaName csaKey) = "Request for article number " ++ (show csaKey) ++ " of section name " ++ csaName
  show (CmdUrlArticle cuaUrl)             = "Request for article from URL " ++ cuaUrl
  show (CmdUnknown)                       = "Request unknown"

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