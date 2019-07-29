{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Avro.Encode
  ( -- * High level interface
    getSchema
  , encodeAvro
  , encodeContainer
  , newSyncBytes
  , encodeContainerWithSync
  -- * Packing containers
  , containerHeaderWithSync
  , packContainerBlocks
  , packContainerBlocksWithSync
  , packContainerValues
  , packContainerValuesWithSync
  -- * Lower level interface
  , EncodeAvro(..)
  , Zag(..)
  , putAvro
  ) where

import qualified Data.Aeson                 as A
import qualified Data.Array                 as Ar
import qualified Data.Binary.IEEE754        as IEEE
import           Data.Bits
import qualified Data.ByteString            as B
import           Data.ByteString.Builder
import           Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.Foldable              as F
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int
import           Data.Ix                    (Ix)
import           Data.List                  as DL
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as U
import           Data.Word
import           Prelude                    as P
import           System.Random.TF.Init      (initTFGen)
import           System.Random.TF.Instances (randoms)

import Data.Avro.Codec
import Data.Avro.EncodeRaw
import Data.Avro.HasAvroSchema
import Data.Avro.Schema        as S
import Data.Avro.Types         as T
import Data.Avro.Zag
import Data.Avro.Zig

encodeAvro :: EncodeAvro a => a -> BL.ByteString
encodeAvro = toLazyByteString . putAvro

-- | Generates a new synchronization marker for encoding Avro containers
newSyncBytes :: IO BL.ByteString
newSyncBytes = BL.pack . DL.take 16 . randoms <$> initTFGen

-- |Encode chunks of objects into a container, using 16 random bytes for
-- the synchronization markers. Blocks are compressed (or not) according
-- to the given `Codec` (`nullCodec` or `deflateCodec`).
encodeContainer :: EncodeAvro a => Codec -> Schema -> [[a]] -> IO BL.ByteString
encodeContainer codec sch xss =
  do sync <- newSyncBytes
     return $ encodeContainerWithSync codec sch sync xss

-- | Creates an Avro container header for a given schema.
containerHeaderWithSync :: Codec -> Schema -> BL.ByteString -> Builder
containerHeaderWithSync codec sch syncBytes =
  lazyByteString avroMagicBytes <> putAvro headers <> lazyByteString syncBytes
  where
    avroMagicBytes :: BL.ByteString
    avroMagicBytes = "Obj" <> BL.pack [1]

    headers :: HashMap Text BL.ByteString
    headers =
      HashMap.fromList
        [
          ("avro.schema", A.encode sch)
        , ("avro.codec", BL.fromStrict (codecName codec))
        ]

-- |Encode chunks of objects into a container, using the provided
-- ByteString as the synchronization markers.
encodeContainerWithSync :: EncodeAvro a => Codec -> Schema -> BL.ByteString -> [[a]] -> BL.ByteString
encodeContainerWithSync codec sch syncBytes xss =
 toLazyByteString $
  containerHeaderWithSync codec sch syncBytes <>
  foldMap putBlocks xss
 where
  putBlocks ys =
    let nrObj    = P.length ys
        nrBytes  = BL.length theBytes
        theBytes = codecCompress codec $ toLazyByteString $ foldMap putAvro ys
    in putAvro nrObj <>
       putAvro nrBytes <>
       lazyByteString theBytes <>
       lazyByteString syncBytes

-- | Packs a new container from a list of already encoded Avro blocks.
-- Each block is denoted as a pair of a number of objects within that block and the block content.
packContainerBlocks :: Codec -> Schema -> [(Int, BL.ByteString)] -> IO BL.ByteString
packContainerBlocks codec sch blocks = do
  sync <- newSyncBytes
  pure $ packContainerBlocksWithSync codec sch sync blocks

-- | Packs a new container from a list of already encoded Avro blocks.
-- Each block is denoted as a pair of a number of objects within that block and the block content.
packContainerBlocksWithSync :: Codec -> Schema -> BL.ByteString -> [(Int, BL.ByteString)] -> BL.ByteString
packContainerBlocksWithSync codec sch syncBytes blocks =
  toLazyByteString $
    containerHeaderWithSync codec sch syncBytes <>
    foldMap putBlock blocks
  where
    putBlock (nrObj, bytes) =
      let compressed = codecCompress codec bytes in
        putAvro nrObj <>
        putAvro (BL.length compressed) <>
        lazyByteString compressed <>
        lazyByteString syncBytes

-- | Packs a container from a given list of already encoded Avro values
-- Each bytestring should represent exactly one one value serialised to Avro.
packContainerValues :: Codec -> Schema -> [[BL.ByteString]] -> IO BL.ByteString
packContainerValues codec sch values = do
  sync <- newSyncBytes
  pure $ packContainerValuesWithSync codec sch sync values

-- | Packs a container from a given list of already encoded Avro values
-- Each bytestring should represent exactly one one value serialised to Avro.
packContainerValuesWithSync :: Codec -> Schema -> BL.ByteString -> [[BL.ByteString]] -> BL.ByteString
packContainerValuesWithSync codec sch syncBytes values =
  toLazyByteString $
    containerHeaderWithSync codec sch syncBytes <>
    foldMap putBlock values
  where
    putBlock ys =
      let nrObj = P.length ys
          nrBytes = BL.length theBytes
          theBytes = codecCompress codec $ toLazyByteString $ mconcat $ lazyByteString <$> ys
      in putAvro nrObj <>
         putAvro nrBytes <>
         lazyByteString theBytes <>
         lazyByteString syncBytes

putAvro :: EncodeAvro a => a -> Builder
putAvro = fst . runAvro . avro

getSchema :: forall a. EncodeAvro a => a -> Schema
getSchema = snd . runAvro . avro

getType :: EncodeAvro a => Proxy a -> Type
getType = getSchema . (asProxyTypeOf undefined)
-- N.B. ^^^ Local knowledge that 'fst' won't be used,
-- so the bottom of 'undefined' will not escape so long as schema creation
-- remains lazy in the argument.

newtype AvroM = AvroM { runAvro :: (Builder,Type) }

class EncodeAvro a where
  avro :: a -> AvroM

avroInt :: forall a. (FiniteBits a, Integral a, EncodeRaw a) => a -> AvroM
avroInt n = AvroM (encodeRaw n, S.Int)

avroLong :: forall a. (FiniteBits a, Integral a, EncodeRaw a) => a -> AvroM
avroLong n = AvroM (encodeRaw n, S.Long)

-- Put a Haskell Int.
putI :: Int -> Builder
putI = encodeRaw

instance EncodeAvro Int  where
  avro = avroInt
instance EncodeAvro Int8  where
  avro = avroInt
instance EncodeAvro Int16  where
  avro = avroInt
instance EncodeAvro Int32  where
  avro = avroInt
instance EncodeAvro Int64  where
  avro = avroInt
instance EncodeAvro Word8 where
  avro = avroInt
instance EncodeAvro Word16 where
  avro = avroInt
instance EncodeAvro Word32 where
  avro = avroLong
instance EncodeAvro Word64 where
  avro = avroLong
instance EncodeAvro Text where
  avro t =
    let bs = T.encodeUtf8 t
    in AvroM (encodeRaw (B.length bs) <> byteString bs, S.String)
instance EncodeAvro TL.Text where
  avro t =
    let bs = TL.encodeUtf8 t
    in AvroM (encodeRaw (BL.length bs) <> lazyByteString bs, S.String)

instance EncodeAvro ByteString where
  avro bs = AvroM (encodeRaw (BL.length bs) <> lazyByteString bs, S.Bytes)

instance EncodeAvro B.ByteString where
  avro bs = AvroM (encodeRaw (B.length bs) <> byteString bs, S.Bytes)

instance EncodeAvro String where
  avro s = let t = T.pack s in avro t

instance EncodeAvro Double where
  avro d = AvroM (word64LE (IEEE.doubleToWord d), S.Double)

instance EncodeAvro Float where
  avro d = AvroM (word32LE (IEEE.floatToWord d), S.Float)

-- Terminating word for array and map types.
long0 :: Builder
long0 = encodeRaw (0 :: Word64)

instance EncodeAvro a => EncodeAvro [a] where
  avro a = AvroM ( if DL.null a then long0 else encodeRaw (F.length a) <> foldMap putAvro a <> long0
                  , S.Array (getType (Proxy :: Proxy a))
                  )

instance (Ix i, EncodeAvro a) => EncodeAvro (Ar.Array i a) where
  avro a = AvroM ( if F.length a == 0 then long0 else encodeRaw (F.length a) <> foldMap putAvro a <> long0
                 , S.Array (getType (Proxy :: Proxy a))
                 )
instance EncodeAvro a => EncodeAvro (V.Vector a) where
  avro a = AvroM ( if V.null a then long0 else encodeRaw (F.length a) <> foldMap putAvro a <> long0
                 , S.Array (getType (Proxy :: Proxy a))
                 )
instance (U.Unbox a, EncodeAvro a) => EncodeAvro (U.Vector a) where
  avro a = AvroM ( if U.null a then long0 else encodeRaw (U.length a) <> foldMap putAvro (U.toList a) <> long0
                 , S.Array (getType (Proxy :: Proxy a))
                 )

instance EncodeAvro a => EncodeAvro (S.Set a) where
  avro a = AvroM ( if S.null a then long0 else encodeRaw (F.length a) <> foldMap putAvro a <> long0
                 , S.Array (getType (Proxy :: Proxy a))
                 )

instance EncodeAvro a => EncodeAvro (HashMap Text a) where
  avro hm = AvroM ( if HashMap.null hm then long0 else putI (F.length hm) <> foldMap putKV (HashMap.toList hm) <> long0
                  , S.Map (getType (Proxy :: Proxy a))
                  )
    where putKV (k,v) = putAvro k <> putAvro v

-- XXX more from containers
-- XXX Unordered containers

-- | Maybe is modeled as a sum type `{null, a}`.
instance EncodeAvro a => EncodeAvro (Maybe a) where
  avro Nothing  = AvroM (putI 0             , S.mkUnion (S.Null:|[S.Int]))
  avro (Just x) = AvroM (putI 1 <> putAvro x, S.mkUnion (S.Null:|[S.Int]))

instance EncodeAvro () where
  avro () = AvroM (mempty, S.Null)

instance EncodeAvro Bool where
  avro b = AvroM (word8 $ fromIntegral $ fromEnum b, S.Boolean)

--------------------------------------------------------------------------------
--  Common Intermediate Representation Encoding

instance EncodeAvro (T.Value Type) where
  avro v =
    case v of
      T.Null      -> avro ()
      T.Boolean b -> avro b
      T.Int i     -> avro i
      T.Long i    -> avro i
      T.Float f   -> avro f
      T.Double d  -> avro d
      T.Bytes bs  -> avro bs
      T.String t  -> avro t
      T.Array vec -> avro vec
      T.Map hm    -> avro hm
      T.Record ty hm ->
        let bs = foldMap putAvro (mapMaybe (`HashMap.lookup` hm) fs)
            fs = P.map fldName (fields ty)
        in AvroM (bs, ty)
      T.Union opts sel val | F.length opts > 0 ->
        case V.elemIndex sel opts of
          Just idx -> AvroM (putI idx <> putAvro val, S.Union opts)
          Nothing  -> error "Union encoding specifies type not found in schema"
      T.Enum sch@S.Enum{..} ix t -> AvroM (putI ix, sch)
      T.Fixed ty bs  ->
        if (B.length bs == size ty)
          then AvroM (byteString bs, S.Bytes)
          else error $ "Fixed type "  <> show (name ty)
                      <> " has size " <> show (size ty)
                      <> " but the value has length " <> show (B.length bs)
