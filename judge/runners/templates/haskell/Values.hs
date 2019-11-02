{-# LANGUAGE FlexibleInstances #-}

module Values where

import System.IO
import Data.String

class Show a => Typeable a where
    toType :: a -> String
    toData :: a -> String
    toData = show

instance Typeable String where toType _ = "text"
instance Typeable Integer where toType _ = "integer"
instance Typeable Float where toType _ = "rational"
instance Typeable Double where toType _ = "rational"
instance Typeable Bool where
    toType _ = "boolean"
    toData a = case a of
        True -> "true"
        False -> "false"

-- First, define the schema of the JSON data
send :: Typeable a => a -> String -> IO ()
send a f = appendFile f ("{ \"data\": " ++ (toData a) ++ ", \"type\": \"" ++ (toType a) ++ "\" }")
