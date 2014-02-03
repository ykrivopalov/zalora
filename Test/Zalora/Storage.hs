{-# LANGUAGE DeriveDataTypeable
           , RecordWildCards
           , TemplateHaskell
           , TypeFamilies #-}

module Test.Zalora.Storage
    ( AcidStorage
    , File(..)
    , Shoe(..)
    , ShoeID(..)
    , StoredShoe(..)
    , provideStorage
    , queryShoe
    , queryAllShoes
    , updateShoe
    ) where

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad.IO.Class
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.Data            ( Data, Typeable )
import Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
import Test.Zalora.Data
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64

$(deriveSafeCopy 0 'base ''File)
$(deriveSafeCopy 0 'base ''Shoe)
$(deriveSafeCopy 0 'base ''ShoeID)

data StoredShoe = StoredShoe
        { storedShoeID :: ShoeID
        , storedShoe :: Shoe
        }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''StoredShoe)

instance IxSet.Indexable StoredShoe where
    empty = ixSet [ ixFun $ \shoe -> [ storedShoeID shoe ] ]


data Storage = Storage {
          storageLastID :: ShoeID
        , storageShoes :: IxSet StoredShoe
        }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Storage)

type AcidStorage = AcidState Storage


initialStorage = Storage (ShoeID 0) empty

getShoe :: ShoeID -> Query Storage (Maybe StoredShoe)
getShoe shoe = do
    shoes <- storageShoes <$> ask
    return $ getOne $ shoes @= shoe

getShoes :: Query Storage [StoredShoe]
getShoes = (IxSet.toList . storageShoes) <$> ask

savePhoto shoe@Shoe{..} =
    case (shoePhoto) of
        (Just (Blob encoded)) ->
            either (\err -> do print $ "photo corrupted " ++ err
                               return $ shoe{shoePhoto = Nothing} )
                   (\decoded -> do
                                   let localFile = "img/" ++ shoeDescription ++ ".jpg"
                                   let ref = shoeDescription ++ ".jpg"
                                   BS.writeFile (localFile) decoded
                                   return shoe{shoePhoto = (Just $ FileRef ref)})
                   (Base64.decode encoded)
        _ -> return $ shoe{shoePhoto = Nothing}

putShoe :: Shoe -> Update Storage ShoeID
putShoe shoe = do
    s@Storage{..} <- get
    let storageLastID' = succ storageLastID
    let shoeRecord = StoredShoe storageLastID' shoe
    put $ s { storageLastID = storageLastID'
            , storageShoes = IxSet.insert shoeRecord storageShoes}
    return storageLastID'

$(makeAcidic ''Storage ['getShoe, 'getShoes, 'putShoe])


provideStorage consumer =
  bracket (openLocalState initialStorage)
          (createCheckpointAndClose)
           (\acid -> consumer acid)

queryShoe acid id = query' acid $ GetShoe id

queryAllShoes acid = query' acid GetShoes

updateShoe acid shoe = do
    shoe' <- liftIO $ savePhoto shoe
    update' acid $ PutShoe shoe'

