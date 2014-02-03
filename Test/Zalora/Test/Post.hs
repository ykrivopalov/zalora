import Data.Aeson
import Data.Functor
import Network.HTTP
import System.Environment
import System.IO
import Test.Zalora.Data
import Test.Zalora.Query
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

makeShoe args@(desc:color:size:photoFile:xs) = do
    photo <- Base64.encode <$> BS.readFile photoFile
    return $ Shoe desc color (read size) $ Just $ Blob photo
makeShoe (desc:color:size:xs) = return $ Shoe desc color (read size) Nothing
makeShoe (desc:color:xs) = return $ Shoe desc (read color) 0 Nothing
makeShoe [desc] = return $ Shoe desc "" 0 Nothing

main = do
    json <- (BSC.unpack . encode) <$> (makeShoe =<< getArgs)
    print $ take 100 json
    simpleHTTP (postRequestWithBody "http://localhost:8000/shoe"
                                    "application/json"
                                    json
               )

