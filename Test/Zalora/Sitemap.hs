{-# LANGUAGE DeriveDataTypeable
           , OverloadedStrings
           , TemplateHaskell
           , TypeOperators #-}

module Test.Zalora.Sitemap (Sitemap(..), sitemap) where

import Control.Category (Category(id, (.)))
import Data.Data
import Data.Maybe
import Data.Text (Text)
import Prelude hiding (id, (.))
import Text.Boomerang.TH (makeBoomerangs)
import Test.Zalora.Storage
import Web.Routes.TH (derivePathInfo)
import Web.Routes.Boomerang
import qualified Data.Text as Text

data Sitemap
    = ShoeGetR ShoeID
    | ShoePutR
    | HomeR
    | ShoesR
    | PhotosR Text
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ShoeID)
$(derivePathInfo ''Sitemap)
$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHomeR
    <> rShoeGetR . (lit "shoe" </> shoeIDmap)
    <> rShoePutR . (lit "shoe")
    <> rShoesR . (lit "shoes")
    <> rPhotosR . (lit "photos" </> photomap)
    )

shoeIDmap :: Router () (ShoeID :- ())
shoeIDmap = xmaph ShoeID (Just . runShoeID) int

photomap :: Router () (Text :- ())
photomap = xmaph id Just (anyText)

