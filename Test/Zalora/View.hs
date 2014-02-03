{-# LANGUAGE OverloadedStrings
           , RecordWildCards #-}

module Test.Zalora.View where

import Data.Monoid
import Data.Text (Text)
import Prelude as Prelude
import Test.Zalora.Storage
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

shoePage shoe@Shoe{..} =
    template "shoe" $ do
        p $ image shoePhoto
        p $ toHtml $ "Description: " ++ shoeDescription
        p $ toHtml $ "Color: " ++ shoeColor
        p $ toHtml $ "Size: " ++ show shoeSize
    where
        image (Just (FileRef path)) = img ! (src $ toValue path)
        image _ = mempty


errorPage :: String -> Html
errorPage text =
    template "error" $ do
        H.p $ H.toHtml $ text

successPage :: String -> Html
successPage text =
    template "success" $ do
        H.p $ H.toHtml $ text

shoesPage shoes =
    template "shoes list" $ table $ mapM_ shoeLine shoes

shoeLine (url, st@StoredShoe{..}) =
    tr $ fields storedShoe
    where
        fields shoe@Shoe{..} = do
            td $ linkToShoe $ toHtml shoeDescription
            td $ toHtml $ shoeColor
            td $ toHtml $ show shoeSize
        linkToShoe = a ! href (toValue url)

template :: Text -> Html -> Html
template title_ body_ =
  html $ do
    H.head $ do
      H.title (toHtml title_)
    body $ do
      body_

