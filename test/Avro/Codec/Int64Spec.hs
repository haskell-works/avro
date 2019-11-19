{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.Int64Spec (spec) where

import           Data.Avro
import           Data.Avro.Encode
import           Data.Avro.Schema
import           Data.Avro.Zig
import           Data.Bits
import           Data.ByteString.Builder
import           Data.Int
import           Data.List.Extra
import           Data.Tagged
import           Data.Word
import           Numeric (showHex)
import           Test.Hspec
import qualified Data.Avro.Types      as AT
import qualified Data.ByteString.Lazy as BL
import qualified Test.QuickCheck      as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}


prettyPrint :: BL.ByteString -> String
prettyPrint = concatMap (`showHex` "") . BL.unpack

newtype OnlyInt64 = OnlyInt64
  { onlyInt64Value :: Int64
  } deriving (Show, Eq)

onlyInt64Schema :: Schema
onlyInt64Schema =
  let fld nm = Field nm [] Nothing Nothing AsIs
   in Record "test.contract.OnlyInt64" [] Nothing Nothing
        [ fld "onlyInt64Value"    Long Nothing
        ]

instance HasAvroSchema OnlyInt64 where
  schema = pure onlyInt64Schema

instance ToAvro OnlyInt64 where
  toAvro sa = record onlyInt64Schema
    [ "onlyInt64Value" .= onlyInt64Value sa
    ]

instance FromAvro OnlyInt64 where
  fromAvro (AT.Record _ r) =
    OnlyInt64  <$> r .: "onlyInt64Value"

bitStringToWord8s :: String -> [Word8]
bitStringToWord8s = reverse . map (toWord . reverse) . chunksOf 8 . reverse . toBinary
  where toBinary :: String -> [Bool]
        toBinary ('1':xs) = True  : toBinary xs
        toBinary ('0':xs) = False : toBinary xs
        toBinary (_  :xs) = toBinary xs
        toBinary       [] = []
        toWord' :: Word8 -> [Bool] -> Word8
        toWord' n (True :bs)  = toWord' ((n `shiftL` 1) .|. 1) bs
        toWord' n (False:bs)  = toWord' ((n `shiftL` 1) .|. 0) bs
        toWord' n _           = n
        toWord = toWord' 0

spec :: Spec
spec = describe "Avro.Codec.Int64Spec" $ do
  it "Can encode 90071992547409917L correctly" $ do
    let expectedBuffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
    let value = OnlyInt64 90071992547409917
    encode value `shouldBe` expectedBuffer
  it "Can decode 90071992547409917L correctly" $ do
    let buffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
    let expectedValue = OnlyInt64 90071992547409917
    decode buffer `shouldBe` Success expectedValue
  it "Can decode encoded Int64 values" $ do
    Q.property $ \(w :: Int64) -> decode (encode (OnlyInt64 w)) == Success (OnlyInt64 w)

  it "Can decode 129L" $ do
    let w = 129 :: Int64
    decode (encode (OnlyInt64 w)) == Success (OnlyInt64 w)

  it "Can decode 36028797018963968 correctly" $ do
    let buffer = BL.pack (bitStringToWord8s "10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001")
    let expectedValue = OnlyInt64 36028797018963968
    decode buffer `shouldBe` Success expectedValue

  it "bitStringToWord8s 00000000"                   $  bitStringToWord8s "00000000"                    `shouldBe` [0x00             ]
  it "bitStringToWord8s 00000001"                   $  bitStringToWord8s "00000001"                    `shouldBe` [0x01             ]
  it "bitStringToWord8s 01111111"                   $  bitStringToWord8s "01111111"                    `shouldBe` [0x7f             ]
  it "bitStringToWord8s 10000000 00000001"          $  bitStringToWord8s "10000000 00000001"           `shouldBe` [0x80, 0x01       ]
  it "bitStringToWord8s 10000001 00000001"          $  bitStringToWord8s "10000001 00000001"           `shouldBe` [0x81, 0x01       ]
  it "bitStringToWord8s 10000010 00000001"          $  bitStringToWord8s "10000010 00000001"           `shouldBe` [0x82, 0x01       ]
  it "bitStringToWord8s 11111111 01111111"          $  bitStringToWord8s "11111111 01111111"           `shouldBe` [0xff, 0x7f       ]
  it "bitStringToWord8s 10000000 10000000 00000001" $  bitStringToWord8s "10000000 10000000 00000001"  `shouldBe` [0x80, 0x80, 0x01 ]
  it "bitStringToWord8s 10000001 10000000 00000001" $  bitStringToWord8s "10000001 10000000 00000001"  `shouldBe` [0x81, 0x80, 0x01 ]
  it "bitStringToWord8s 10000001 10000000 00000000" $  bitStringToWord8s "10000001 10000000 00000000"  `shouldBe` [0x81, 0x80, 0x00 ]

  it "Can zig" $ do
    zig (          0 :: Int64)  `shouldBe` 0
    zig (         -1 :: Int64)  `shouldBe` 1
    zig (          1 :: Int64)  `shouldBe` 2
    zig (         -2 :: Int64)  `shouldBe` 3
    zig ( 2147483647 :: Int64)  `shouldBe` 4294967294
    zig (-2147483648 :: Int64)  `shouldBe` 4294967295
