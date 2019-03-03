{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Avro encoding and decoding routines.
--
-- This library provides a high level interface for encoding (and decoding)
-- Haskell values in Apache's Avro serialization format.  The goal is to
-- match Aeson's API whenever reasonable, meaning user experience with one
-- effectively translate to the other.
--
-- Avro RPC is not currently supported.
--
-- **Library Structure**
--
-- The library structure includes:
--   * This module, 'Data.Avro', providing a high-level interface via
--     classes of 'FromAvro' and 'ToAvro' for decoding and encoding values.
--   * 'Data.Avro.Type' define the types of Avro data, providing a common
--      (intermediate) representation for any data that is encoded or decoded
--      by Data.Avro.
--   * 'Data.Avro.Encode' and 'Data.Avro.Decode': More
--     efficient conversion capable of avoiding the intermediate representation.
--     Also, the implementation of the en/decoding of the intermediate
--     representation.
--   * 'Data.Avro.Deconflict': translate decoded data from an
--     encoder schema to the (potentially different) decoder's schema.
--   * 'Data.Avro.Schema': Defines the type for Avro schema's and its JSON
--      encoding/decoding.
--
-- Example decoding:
--
-- Let's say you have an ADT and related schema:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import qualified Data.Avro.Types as Ty
-- import Data.Avro.Schema
-- import Data.Avro
-- import           Data.List.NonEmpty (NonEmpty(..))
--
-- data MyEnum = A | B | C | D deriving (Eq,Ord,Show,Enum,Generic)
-- data MyStruct = MyStruct (Either MyEnum String) Int
--
-- meSchema :: Schema
-- meSchema = Schema $ mkEnum "MyEnum" [] Nothing Nothing ["A","B","C","D"]
--
-- msSchema  :: Schema
-- msSchema =
--   Struct "MyStruct" Nothing [] Nothing Nothing
--       [ fld "enumOrString" eOrS (Just $ String "The Default")
--       , fld "int" Int (Just (Ty.Int 1))
--       ]
--      where
--      fld nm ty def = Field nm [] Nothing Nothing ty def
--      eOrS = mkUnion (meSchema :| [String])
--
-- instance ToAvro MyEnum where
--     toAvro = toAvroEnum
-- instance ToAvro MyStruct where
--     toAvro (MyStruct ab i) =
--      record [ "enumOrString" .= ab
--             , "int"          .= i
--             ]
--
-- main = do
--   let val = MyStruct (Right "Hello") 1
--   print (fromAvro (toAvro val) == Success val)
--
-- @
module Data.Avro
  ( FromAvro(..)
  , ToAvro(..)
  , HasAvroSchema(..)
  , Schema
  , Avro
  , (.:)
  , (.=), record, fixed
  , Result(..), badValue
  , decode
  , decodeWithSchema
  , decodeContainer
  , decodeContainerWithSchema
  , decodeContainerBytes
  , encode
  , encodeContainer
  , encodeContainerWithSync
  , schemaOf
  ) where

import           Control.Arrow         (first)
import qualified Data.Avro.Decode      as D
import qualified Data.Avro.Decode.Lazy as DL
import           Data.Avro.Deconflict  as C
import qualified Data.Avro.Encode      as E
import           Data.Avro.Schema      as S
import           Data.Avro.Types       as T
import qualified Data.Binary.Get       as G
import qualified Data.Binary.Put       as P
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as BL
import           Data.Foldable         (toList)
import qualified Data.HashMap.Strict   as HashMap
import           Data.Int
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.Map              as Map
import           Data.Monoid           ((<>))
import           Data.Tagged
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as TL
import qualified Data.Vector           as V
import           Data.Word
import           Prelude               as P

import Data.Avro.Codec (nullCodec)
import Data.Avro.FromAvro
import Data.Avro.HasAvroSchema
import Data.Avro.ToAvro

type Avro a = (FromAvro a, ToAvro a)

-- | Decode a lazy bytestring using a Schema for the return type.
decode :: forall a. FromAvro a => ByteString -> Result a
decode bytes =
  case D.decodeAvro (untag (schema :: Tagged a Type)) bytes of
      Right val -> fromAvro val
      Left err  -> Error err

-- | Decode a lazy bytestring with a provided schema
decodeWithSchema :: FromAvro a => Schema -> ByteString -> Result a
decodeWithSchema sch bytes =
  case D.decodeAvro sch bytes of
    Right val -> fromAvro val
    Left err  -> Error err

-- | Decode a container and de-conflict the writer schema with
-- a reader schema for a return type.
-- Like in 'decodeContainerWithSchema'
-- exceptions are thrown instead of a 'Result' type to
-- allow this function to be read lazy (to be done in some later version).
decodeContainer :: forall a. FromAvro a => ByteString -> [[a]]
decodeContainer bs =
  let readerSchema = untag (schema :: Tagged a Schema)
  in decodeContainerWithSchema readerSchema bs

-- |Decode a container and de-conflict the writer schema with a given
-- reader-schema.  Exceptions are thrown instead of a 'Result' type to
-- allow this function to be read lazy (to be done in some later version).
decodeContainerWithSchema :: FromAvro a => Schema -> ByteString -> [[a]]
decodeContainerWithSchema readerSchema bs =
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

-- | Encodes a value to a lazy ByteString
encode :: ToAvro a => a -> BL.ByteString
encode = E.encodeAvro . toAvro

-- | Encode chunks of objects into a container, using 16 random bytes for
-- the synchronization markers.
encodeContainer :: forall a. ToAvro a => [[a]] -> IO BL.ByteString
encodeContainer =
  let sch = untag (schema :: Tagged a Schema)
  in E.encodeContainer nullCodec sch . map (map toAvro)

-- | Encode chunks of objects into a container, using the provided
-- ByteString as the synchronization markers.
encodeContainerWithSync :: forall a. ToAvro a => (Word64,Word64,Word64,Word64) -> [[a]] -> BL.ByteString
encodeContainerWithSync (a,b,c,d) =
  let
    sch = untag (schema :: Tagged a Schema)
    syncBytes = P.runPut $ mapM_ P.putWord64le [a,b,c,d]
  in E.encodeContainerWithSync nullCodec sch syncBytes . map (map toAvro)

-- |Like 'decodeContainer' but returns the avro-encoded bytes for each
-- object in the container instead of the Haskell type.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
decodeContainerBytes :: ByteString -> [[ByteString]]
decodeContainerBytes bs =
  case D.decodeContainerWith schemaBytes bs of
    Right (writerSchema, val) -> val
    Left e                    -> error $ "Could not decode container: " <> e
  where
  schemaBytes sch =
    do start <- G.bytesRead
       end   <- G.lookAhead $ do _ <- D.getAvroOf sch
                                 G.bytesRead
       G.getLazyByteString (end-start)

record :: Foldable f => Type -> f (Text,T.Value Type) -> T.Value Type
record ty = T.Record ty . HashMap.fromList . toList

fixed :: Type -> B.ByteString -> T.Value Type
fixed = T.Fixed
-- @enumToAvro val@ will generate an Avro encoded value of enum suitable
-- for serialization ('encode').
-- enumToAvro :: (Show a, Enum a, Bounded a, Generic a) => a -> T.Value Type
-- enumToAvro e = T.Enum ty (show e)
--  where
--   ty = S.Enum nm Nothing [] Nothing (map (Text.pack . show) [minBound..maxBound])
--   nm = datatypeName g
--   g  = from e -- GHC generics
