{-# LANGUAGE OverloadedStrings
           , RecordWildCards #-}

module Test.Zalora.Query where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Test.Zalora.Data

import qualified Data.ByteString as BS
import qualified Data.Text as T

instance FromJSON Shoe where
    parseJSON (Object v) =
        Shoe <$> v .: "description"
             <*> v .: "color"
             <*> v .: "size"
             <*> ((Just . Blob . BS.pack) <$> (v .: "photo"))

    parseJSON _ = mzero

instance ToJSON Shoe where
    toJSON s@Shoe{..} =
        object [ "description" .= shoeDescription
               , "color" .= shoeColor
               , "size" .= shoeSize
               , "photo" .= (convertPhoto $ shoePhoto)
               ]
        where convertPhoto (Just (Blob b)) = toJSON $ BS.unpack b
              convertPhoto _ = toJSON ("" :: String)

instance ToJSON File where
    toJSON file =
        case file of
            (Blob b) -> toJSON $ BS.unpack b
            _ -> toJSON ("" :: String)
