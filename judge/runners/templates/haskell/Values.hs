{-# LANGUAGE FlexibleInstances #-}

module Values where

import System.IO
import Data.String

class Show a => Typeable a where
    toType :: a -> String
    toData :: a -> String
    toData = show

to_string :: Bool -> String
to_string b = case b of
    True -> "true"
    False -> "false"

instance Typeable String where toType _ = "text"
instance Typeable Integer where toType _ = "integer"
instance Typeable Float where toType _ = "rational"
instance Typeable Double where toType _ = "rational"
instance Typeable Bool where
    toType _ = "boolean"
    toData a = to_string a

-- First, define the schema of the JSON data
send :: Typeable a => a -> String -> IO ()
send a f = appendFile f ("{ \"data\": " ++ (toData a) ++ ", \"type\": \"" ++ (toType a) ++ "\" }")

evaluated :: String -> Bool -> String -> IO ()
evaluated f a s = appendFile f ("{ \"string\": \"" ++ s ++ "\", \"type\": \"evaluated\", \"accepted\": " ++ (to_string a) ++ " }")
