module DecodeContainer
where

import qualified Data.Aeson           as A
import           Data.Avro.Decode
import           Data.Avro.Deconflict as D
import qualified Data.ByteString.Lazy as BL
import           System.Environment

main :: IO ()
main =
  do (file:rest) <- getArgs
     cont <- BL.readFile file
     case decodeContainer cont of
      Left e -> print e
      Right (s,v) ->
        do putStrLn $ "Schema: " ++ show s
           putStrLn "--------------------------------------------------"
           print v
           case rest of
            [schFile] -> do
              putStrLn "---- DECONFLICTED ------"
              Just readerSchema <- A.decode <$> BL.readFile schFile
              print (map (map (D.deconflict s readerSchema)) v)
            _ -> return ()
