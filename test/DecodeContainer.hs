import Data.Avro.Decode
import qualified Data.ByteString.Lazy as BL
import System.Environment
import qualified Data.Aeson as A

main :: IO ()
main =
  do [file] <- getArgs
     cont <- BL.readFile file
     print $ decodeContainer cont
