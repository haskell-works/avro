{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Avro.Types as Ty
import Data.Avro.Schema
import Data.Avro
import Data.Text (Text)
import           Data.List.NonEmpty (NonEmpty(..))

data MyEnum = A | B | C | D deriving (Eq,Ord,Show,Enum)
data MyStruct = MyStruct (Either MyEnum Text) Int deriving (Eq,Ord,Show)

meSchema :: Schema
meSchema = mkEnum "MyEnum" [] Nothing Nothing ["A","B","C","D"]

msSchema  :: Schema
msSchema =
  Record "MyStruct" Nothing [] Nothing Nothing
      [ fld "enumOrString" eOrS (Just $ Ty.String "The Default")
      , fld "intvalue" Long (Just (Ty.Long 1))
      ]
     where
     fld nm ty def = Field nm [] Nothing Nothing ty def
     eOrS = mkUnion (meSchema :| [String])

instance ToAvro MyEnum where
    toAvro = toAvro . fromEnum
instance ToAvro MyStruct where
    toAvro (MyStruct ab i) =
     record [ "enumOrString" .= ab
            , "intvalue"     .= i
            ]
instance FromAvro MyStruct where
    fromAvro (Ty.Record r) =
        MyStruct <$> r .: "enumOrString"
                 <*> r .: "intvalue"
instance FromAvro MyEnum where
    fromAvro x = toEnum <$> fromAvro x

main = do
  let val = MyStruct (Right "Hello") 1
      enc = toAvro val
  print val
  print enc
  print (fromAvro enc `asTypeOf` Success val)
  print (fromAvro enc == Success val)
