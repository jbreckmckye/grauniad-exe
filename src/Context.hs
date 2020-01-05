module Context
    ( apiKey
    )
where

import System.Environment ( lookupEnv )
import Control.Exception ( Exception, throw )

data ContextException
    = MissingEnv { key :: String }

instance Exception ContextException
instance Show ContextException where
    show (MissingEnv key) = "Missing environment variable: " ++ key

apiKey :: IO String
apiKey = envOrFail "GRAUNIAD_API_KEY"

envOrFail :: String -> IO String
envOrFail key = do
    env <- lookupEnv key
    case env of
        Just value -> return value
        Nothing    -> throw $ MissingEnv key