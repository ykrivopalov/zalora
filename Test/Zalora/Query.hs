{-# LANGUAGE OverloadedStrings
           , RecordWildCards #-}

module Test.Zalora.Query where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Test.Zalora.Data

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

instance FromJSON Shoe where
    parseJSON (Object v) =
        Shoe <$> v .: "description"
             <*> v .: "color"
             <*> (read <$> v .: "size")
             <*> ((Just . Blob . BSC.pack) <$> v .: "photo")

    parseJSON _ = mzero

instance ToJSON Shoe where
    toJSON s@Shoe{..} =
        object [ "description" .= shoeDescription
               , "color" .= shoeColor
               , "size" .= show shoeSize
               , "photo" .= (convertPhoto $ shoePhoto)
               ]
        where convertPhoto (Just (Blob b)) = toJSON $ BSC.unpack b
              convertPhoto _ = toJSON ("" :: String)
