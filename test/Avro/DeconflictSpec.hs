{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.DeconflictSpec
where

import Control.Monad.IO.Class
import Data.Avro              as A
import Data.Avro.Deconflict
import Data.Avro.Deriving
import Data.Avro.Schema
import Data.Either
import Data.List.NonEmpty     (NonEmpty (..))

import qualified Avro.Deconflict.A.Reader         as AR
import qualified Avro.Deconflict.A.Writer         as AW
import qualified Avro.Deconflict.B.Reader         as BR
import qualified Avro.Deconflict.B.Writer         as BW
import qualified Data.Avro.Decode                 as A (decodeAvro)
import qualified Data.Avro.Decode.Lazy            as AL
import qualified Data.Avro.Decode.Lazy.Deconflict as AL
import qualified Data.Avro.Types                  as Ty

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

writerMessage :: AW.Outer
writerMessage = AW.Outer "Peone" (AW.Inner 3) (AW.Inner 5)

spec :: Spec
spec = describe "Avro.DeconflictSpec" $ do
  it "should deconflict simple message" $ do
    let payload = A.encode $ AW.Inner 3
    let Right decodedAvro = A.decodeAvro AW.schema'Inner payload
    let Right deconflicted = deconflict AW.schema'Inner AR.schema'Inner decodedAvro
    fromAvro deconflicted `shouldBe` Success (AR.Inner 3 Nothing)

  it "should deconflict nested message" $ do
    let payload = A.encode writerMessage
    let Right decodedAvro = A.decodeAvro AW.schema'Outer payload
    let Right deconflicted = deconflict AW.schema'Outer AR.schema'Outer decodedAvro

    fromAvro deconflicted `shouldBe` Success (AR.Outer "Peone" (AR.Inner 3 Nothing) (AR.Inner 5 Nothing))


  it "should deconflict nested message lazily" $ do
    let payload = A.encode writerMessage
    let decodedAvro = AL.decodeAvro AW.schema'Outer payload
    let deconflicted = AL.deconflict AW.schema'Outer AR.schema'Outer decodedAvro

    AL.fromLazyAvro deconflicted `shouldBe` Success (AR.Outer "Peone" (AR.Inner 3 Nothing) (AR.Inner 5 Nothing))

  it "should deconflict within nested union" $ do
    w <- liftIO $ A.encodeContainer
      [ [ BW.Foo
          { BW.fooFieldA = Just BW.Goo
            { BW.gooFieldB1  = BW.Moo
              { BW.mooName   = "X"
              }
            , BW.gooFieldB2  = BW.Moo
              { BW.mooName   = "X"
              }
            }
          }
        ]
      ]

    let r = AL.decodeContainer w :: [Either String BR.Foo]

    r `shouldBe`
      [ Right BR.Foo
        { BR.fooFieldA = Just BR.Goo
          { BR.gooFieldB1 = BR.Moo
            { BR.mooName     = "X"
            , BR.mooFullName = Nothing
            }
          , BR.gooFieldB2 = BR.Moo
            { BR.mooName     = "X"
            , BR.mooFullName = Nothing
            }
          }
        }
      ]
