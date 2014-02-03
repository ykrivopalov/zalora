{-# LANGUAGE OverloadedStrings
           , RecordWildCards #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (msum)
import Control.Monad.IO.Class
import Data.Aeson (eitherDecode)
import Data.Maybe
import Happstack.Server
import Test.Zalora.Data
import Test.Zalora.Log
import Test.Zalora.Query
import Test.Zalora.Sitemap
import Test.Zalora.Storage
import Test.Zalora.View
import Web.Routes (RouteT, Site(..), runRouteT, setDefault, showURL)
import Web.Routes.Boomerang (boomerangSite)
import Web.Routes.Happstack (implSite)
import qualified Data.Text as Text

serverPort = 8000

main = do
    provideStorage $ (\acid -> simpleHTTP nullConf{port=serverPort} (shoeApp acid))

shoeApp :: AcidStorage -> ServerPart Response
shoeApp acid =
    do decodeBody policy
       msum [ implSite serverPath "" $ site acid
            , notFound $ toResponse $ errorPage "page not found"
            ]
    where serverPath = Text.pack $ "http://localhost:" ++ show serverPort

policy :: BodyPolicy
policy = (defaultBodyPolicy "/tmp/" 1000000 1000000 1000000)


site :: AcidStorage -> Site Sitemap (ServerPartT IO Response)
site acid = setDefault HomeR $ boomerangSite (runRouteT (route acid)) sitemap

route :: AcidStorage -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route acid url =
    case url of
        ShoeGetR id -> shoeGet acid id
        ShoePutR -> shoePut acid
        ShoesR -> shoesGet acid
        PhotosR path -> shoesPhoto path
        _ -> notFound $ toResponse $ errorPage "not found"

shoeGet acid id = do
    method GET
    liftIO $ print ("GET", id)
    mShoe <- queryShoe acid id
    maybe (notFound $ toResponse $ errorPage "shoe not found")
          (\(s@StoredShoe{..}) -> do
               shoe <- fixPhotoRef storedShoe
               ok $ toResponse $ shoePage shoe)
          mShoe
    where
        fixPhotoRef shoe@Shoe{..} =
            maybe (return shoe{shoePhoto=Nothing})
                  (\photo -> case photo of
                           FileRef ref -> do
                               url <- showURL $ PhotosR $ Text.pack ref
                               return shoe{shoePhoto=(Just $ FileRef $ Text.unpack url)}
                           _ -> return shoe{shoePhoto=Nothing}
                  )
                  shoePhoto

shoePut acid = do
    method POST
    reqBody <- takeRequestBody =<< askRq
    maybe (responseError "request body not exist")
          (\(Body body) -> decodeReq body)
          reqBody
    where responseError text =
              badRequest $ toResponse $ errorPage $ "post shoe failed: " ++ text
          decodeReq body =
               either
                 (\err -> do
                      liftIO $ print err
                      responseError err
                 )
                 (\shoe -> do
                      liftIO $ printShort ("POST ", shoe)
                      shoe' <- updateShoe acid $ shoe
                      ok $ toResponse $ successPage $ "shoe posted: " ++ show shoe'
                 )
                 (eitherDecode body)

shoesGet acid = do
    method GET
    liftIO $ print "GET shoes"
    shoes <- mapM addURL =<< queryAllShoes acid
    ok $ toResponse $ shoesPage shoes
    where
        addURL shoe@StoredShoe{..} = do
            url <- showURL $ ShoeGetR storedShoeID
            return (url, shoe)

shoesPhoto path = do
    method GET
    serveFile (asContentType "image/jpeg") $ "img/" ++ Text.unpack path

