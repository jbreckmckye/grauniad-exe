module Cmd (
  Cmd,
  cmdFromArgs
) where

import System.Environment
import Data.Text


-- Types
-- ----------------------------------------------------------------------

data Cmd
  = CmdHeadlines
  | CmdSectionArticle   { csaName :: Text, csaKey :: Integer }
  | CmdUrlArticle       { cuaUrl :: Text }
  | CmdUnknown

instance Show Cmd where
  show (CmdHeadlines)                     = "Request for the latest headlines"
  show (CmdSectionArticle csaName csaKey) = "Request for article number " ++ (show csaKey) ++ " of section name " ++ (unpack csaName)
  show (CmdUrlArticle cuaUrl)             = "Request for article from URL " ++ (unpack cuaUrl)
  show (CmdUnknown)                       = "Request unknown"


-- Operations
-- ----------------------------------------------------------------------

cmdFromArgs :: IO Cmd
cmdFromArgs = do
  args <- getArgs
  return $ extract args


extract :: [String] -> Cmd
extract args = case args of
  [ ]                   -> CmdHeadlines
  [urlStr]              -> CmdUrlArticle (pack urlStr)
  [sectionStr, keyStr]  -> CmdSectionArticle (pack sectionStr) (read keyStr)
  unknown               -> CmdUnknown
