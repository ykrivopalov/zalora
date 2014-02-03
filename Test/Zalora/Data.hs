{-# LANGUAGE DeriveDataTypeable
           , GeneralizedNewtypeDeriving #-}

module Test.Zalora.Data where

import Data.Data
import qualified Data.ByteString as BS

data File = Blob BS.ByteString | FileRef String
    deriving (Data, Eq, Ord, Read, Show, Typeable)

data Shoe = Shoe { shoeDescription :: String
                 , shoeColor :: String
                 , shoeSize :: Integer
                 , shoePhoto :: Maybe File
                 }
    deriving (Data, Eq, Ord, Read, Show, Typeable)

newtype ShoeID = ShoeID { runShoeID :: Int }
    deriving (Data, Enum, Eq, Ord, Read, Show, Typeable)
