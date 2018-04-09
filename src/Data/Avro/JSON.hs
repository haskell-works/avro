{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.JSON where

import           Data.Semigroup       ((<>))

import qualified Data.Aeson           as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as HashMap
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Tagged
import qualified Data.Text            as Text

import           Data.Avro            (FromAvro (..), Result (..), ToAvro (..))
import qualified Data.Avro            as Avro
import           Data.Avro.Schema     (Schema, parseAvroJSON)
import qualified Data.Avro.Schema     as Schema
import qualified Data.Avro.Types      as Avro

fromJSON :: forall a. (FromAvro a) => Aeson.Value -> Result a
fromJSON json = parseAvroJSON union env schema json >>= fromAvro
  where schema = untag (Avro.schema :: Tagged a Schema)
        env = Schema.buildTypeEnvironment missing schema . Schema.TN
        missing name = fail $ "Type " <> show name <> " not in schema."

        union (Schema.Union schemas _) Aeson.Null
          | Schema.Null `elem` schemas = pure $ Avro.Union schemas Schema.Null Avro.Null
          | otherwise                  = fail "Null not in union."
        union (Schema.Union schemas _) (Aeson.Object obj)
          | length obj < 1 = fail "Invalid encoding of union: empty object ({})."
          | length obj > 1 = fail "Invalid encoding of union: object with too many fields."
          | otherwise      =
            let branch = head $ HashMap.keys obj
                names = HashMap.fromList [(Schema.typeName t, t) | t <- NE.toList schemas]
            in case HashMap.lookup branch names of
              Just t  -> do
                nested <- parseAvroJSON union env t $ obj ! branch
                return $ Avro.Union schemas t nested
              Nothing -> fail $ "Type '" <> Text.unpack branch <> "' not in union: " <> show schemas
        union Schema.Union{} _ =
          Avro.Error "Invalid JSON representation for union: has to be a JSON object with exactly one field."
        union _ _ = error "Impossible: function given non-union schema."

parseJSON :: forall a. (FromAvro a) => ByteString -> Result a
parseJSON input = case Aeson.eitherDecode input of
  Left msg    -> Error msg
  Right value -> fromJSON value

toJSON :: forall a. (ToAvro a) => a -> Aeson.Value
toJSON = Aeson.toJSON . toAvro
