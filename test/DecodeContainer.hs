import Data.Avro.Decode
import qualified Data.ByteString.Lazy as BL
import System.Environment
import qualified Data.Aeson as A

main :: IO ()
main =
  do [file] <- getArgs
     cont <- BL.readFile file
     case decodeContainer cont of
      Left e -> print e
      Right (s,v) ->
        do putStrLn $ "Schema: " ++ show s
           putStrLn "--------------------------------------------------"
           print v
