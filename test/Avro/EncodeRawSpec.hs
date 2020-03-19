module Avro.EncodeRawSpec (spec) where

import           Data.Avro.Internal.EncodeRaw
import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy         as BL
import           Data.List.Extra
import           Data.Word
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

bitStringToWord8s :: String -> [Word8]
bitStringToWord8s = reverse . map (toWord . reverse) . chunksOf 8 . reverse . toBinary
  where toBinary :: String -> [Bool]
        toBinary ('1':xs) = True  : toBinary xs
        toBinary ('0':xs) = False : toBinary xs
        toBinary (_  :xs) = toBinary xs
        toBinary       [] = []
        toWord' :: Word8 -> [Bool] -> Word8
        toWord' n (True :bs) = toWord' ((n `shiftL` 1) .|. 1) bs
        toWord' n (False:bs) = toWord' ((n `shiftL` 1) .|. 0) bs
        toWord' n _          = n
        toWord = toWord' 0

spec :: Spec
spec = describe "Avro.EncodeRawSpec" $ do
  it "Can encodeRaw (                  0 :: Word64)" $ toLazyByteString (encodeRaw (                  0 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                                 00000000" )
  it "Can encodeRaw (                  1 :: Word64)" $ toLazyByteString (encodeRaw (                  1 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                                 00000001" )
  it "Can encodeRaw (                  2 :: Word64)" $ toLazyByteString (encodeRaw (                  2 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                                 00000010" )
  it "Can encodeRaw (                127 :: Word64)" $ toLazyByteString (encodeRaw (                127 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                                 01111111" )
  it "Can encodeRaw (                129 :: Word64)" $ toLazyByteString (encodeRaw (                129 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                        10000001 00000001" )
  it "Can encodeRaw (                130 :: Word64)" $ toLazyByteString (encodeRaw (                130 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                        10000010 00000001" )
  it "Can encodeRaw (              16383 :: Word64)" $ toLazyByteString (encodeRaw (              16383 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                        11111111 01111111" )
  it "Can encodeRaw (              16385 :: Word64)" $ toLazyByteString (encodeRaw (              16385 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                               10000001 10000000 00000001" )
  it "Can encodeRaw (            2097153 :: Word64)" $ toLazyByteString (encodeRaw (            2097153 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                      10000001 10000000 10000000 00000001" )
  it "Can encodeRaw (          268435457 :: Word64)" $ toLazyByteString (encodeRaw (          268435457 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                             10000001 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (        34359738369 :: Word64)" $ toLazyByteString (encodeRaw (        34359738369 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                    10000001 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (      4398046511105 :: Word64)" $ toLazyByteString (encodeRaw (      4398046511105 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                           10000001 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (    562949953421313 :: Word64)" $ toLazyByteString (encodeRaw (    562949953421313 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                  10000001 10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (  72057594037927937 :: Word64)" $ toLazyByteString (encodeRaw (  72057594037927937 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "         10000001 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (9223372036854775809 :: Word64)" $ toLazyByteString (encodeRaw (9223372036854775809 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "10000001 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (                128 :: Word64)" $ toLazyByteString (encodeRaw (                128 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                                        10000000 00000001" )
  it "Can encodeRaw (              16384 :: Word64)" $ toLazyByteString (encodeRaw (              16384 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                               10000000 10000000 00000001" )
  it "Can encodeRaw (            2097153 :: Word64)" $ toLazyByteString (encodeRaw (            2097152 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                                      10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (          268435457 :: Word64)" $ toLazyByteString (encodeRaw (          268435456 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                             10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (        34359738369 :: Word64)" $ toLazyByteString (encodeRaw (        34359738368 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                                    10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (      4398046511105 :: Word64)" $ toLazyByteString (encodeRaw (      4398046511104 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                           10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (    562949953421313 :: Word64)" $ toLazyByteString (encodeRaw (    562949953421312 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "                  10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (  72057594037927936 :: Word64)" $ toLazyByteString (encodeRaw (  72057594037927936 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "         10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
  it "Can encodeRaw (9223372036854775808 :: Word64)" $ toLazyByteString (encodeRaw (9223372036854775808 :: Word64)) `shouldBe` BL.pack (bitStringToWord8s "10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001" )
