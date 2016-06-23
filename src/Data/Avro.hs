-- | Avro encoding and decoding routines.
module Data.Avro
  ( FromAvro(..)
  , Result(..)
  , ToAvro(..)
  , decode
  , decodeContainer
  -- , encode
  -- , encodeContainer
  ) where

import Prelude as P
import Data.Avro.Encode as E
import qualified Data.Avro.Decode as D
import Data.Avro.Schema as S
import Data.Avro.Types  as T
import Data.Avro.Deconflict as C
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

decode :: FromAvro a => Schema -> ByteString -> Result a
decode sch bytes =
  case D.decodeAvro sch bytes of
    Right val -> fromAvro val
    Left err  -> Error err

decodeContainer :: FromAvro a => Schema -> ByteString -> [[a]]
decodeContainer readerSchema bs =
  case D.decodeContainer bs of
    Right (writerSchema,val) ->
      let err e = error $ "Could not deconflict reader and writer schema." <> e
          dec x =
            case C.deconflict writerSchema readerSchema x of
              Left e   -> err e
              Right v  -> case fromAvro v of
                            Success x -> x
                            Error e   -> error e
      in P.map (P.map dec) val
    Left err -> error err

class FromAvro a where
  fromAvro :: Value Type -> Result a

class ToAvro a where
  toAvro :: a -> T.Value Type
