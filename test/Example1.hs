{-# LANGUAGE OverloadedStrings #-}
module Example1
where
import           Data.Avro
import           Data.Avro.Schema
import qualified Data.Avro.Types      as Ty
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as Text

data MyEnum = A | B | C | D deriving (Eq,Ord,Show,Enum)
data MyStruct = MyStruct (Either MyEnum Text) Int deriving (Eq,Ord,Show)

-- Explicit 'Schema' types that specify the Avro encoding of a structure.
-- Schema's often come from an external JSON definition (.avsc files) or
-- embedded in object files.
meSchema :: Schema
meSchema = mkEnum "MyEnum" [] Nothing ["A","B","C","D"]

msSchema  :: Schema
msSchema =
  Record "MyStruct" [] Nothing Nothing
      [ fld "enumOrString" eOrS (Just $ Ty.String "The Default")
      , fld "intvalue" (Long ReadAsIs) Nothing
      ]
     where
     fld nm ty def = Field nm [] Nothing Nothing AsIs ty def
     eOrS = mkUnion (meSchema :| [String])

-- Encoding data, via the ToAvro class, requires both the routine that encodes
-- data as well as the schema under which it is encoded.  The encoding and
-- schema must match, though there is no type or programmatic routine that enforces
-- this law.
instance HasAvroSchema MyEnum where
    schema = pure meSchema

instance ToAvro MyEnum where
    toAvro x = Ty.Enum meSchema (fromEnum x) (Text.pack $ show x)
    -- schema = pure meSchema

instance HasAvroSchema MyStruct where
    schema = pure msSchema

instance ToAvro MyStruct where
    toAvro (MyStruct ab i) =
     record msSchema
            [ "enumOrString" .= ab
            , "intvalue"     .= i
            ]
    -- schema = pure msSchema

-- Much like Aeson, decoding data is involves pattern matching the value
-- constructor then building the ADT.
instance FromAvro MyStruct where
    fromAvro (Ty.Record _fs r) =
        MyStruct <$> r .: "enumOrString"
                 <*> r .: "intvalue"
    fromAvro v = badValue v "MyStruct"

instance FromAvro MyEnum where
    fromAvro (Ty.Enum _ i _) = pure (toEnum i)
    fromAvro v               = badValue v "MyEnum"

main = do
  let valR = MyStruct (Right "Hello") 1
      encR = toAvro valR
      valL = MyStruct (Left C) (negate 1944)
      encL = toAvro valL
  putStrLn "----------- MS Right value -------------"
  print (fromAvro encR `asTypeOf` Success valR)
  print (fromAvro encR == Success valR)
  putStrLn "----------- MS Left value --------------"
  print (fromAvro encL `asTypeOf` Success valL)
  print (fromAvro encL == Success valL)
  putStrLn "----------- MS Right full bytestring enc/dec--------------"
  print (BL.unpack $ encode valR)
  print (decode (encode valR) `asTypeOf` Success valR)
  print (decode (encode valR) == Success valR)
  putStrLn "----------- MS Left full bytestring enc/dec--------------"
  print (BL.unpack $ encode valL)
  print (decode (encode valL) `asTypeOf` Success valL)
  print (decode (encode valL) == Success valL)
