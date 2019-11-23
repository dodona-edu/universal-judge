{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Values where

import Data.Aeson
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LBS


class ToJSON a => Typeable a where
    toType :: a -> String
    toType a = "unknown"
    toJson :: a -> Value
    toJson = toJSON


-- We support maybe as the null value
instance Typeable a => Typeable (Maybe a) where
    toType m = case m of
        Just n -> toType n
        Nothing -> "nothing"
    toJson m = case m of
        Just n -> toJson n
        Nothing -> Null
instance {-# OVERLAPPABLE #-} Typeable String where toType _ = "text"
instance Typeable Text where toType _ = "text"
instance Typeable Integer where toType _ = "integer"
instance Typeable Float where toType _ = "rational"
instance Typeable Double where toType _ = "rational"
instance Typeable Bool where toType _ = "boolean"
instance {-# OVERLAPPABLE #-} Typeable a => Typeable [a] where
    toType m = "list"
    toJson l = toJSON (map toJson l)
instance Typeable a => Typeable (Set a) where
    toType m = "set"
    toJson s = toJSON (map toJson (Set.toList s))


sendValue :: Typeable a => FilePath -> a -> IO ()
sendValue file value = LBS.appendFile file (encode (object [
        "type" .= toJSON (toType value),
        "data" .= toJson value
    ]))


sendSpecificEvaluated :: FilePath -> Bool -> String -> String -> [String] -> IO ()
sendSpecificEvaluated file result expected actual messages =
    LBS.appendFile file (encode (object [
        "result" .= toJSON result,
        "readable_expected" .= toJSON expected,
        "readable_actual" .= toJSON actual,
        "messages" .= toJSON messages
    ]))


sendCustomEvaluated :: FilePath -> Bool -> [String] -> IO ()
sendCustomEvaluated file result messages =
    LBS.appendFile file (encode (object [
        "result" .= toJSON result,
        "messages" .= toJSON messages
    ]))
