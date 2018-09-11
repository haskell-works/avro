{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Avro.EitherN where

import           Data.Avro
import           Data.Avro.Schema
import qualified Data.Avro.Types    as T
import           Data.Tagged
import           Data.List.NonEmpty
import           GHC.Generics       (Generic)

data Either3 a b c = E3_1 a | E3_2 b | E3_3 c deriving (Eq, Ord, Show, Generic)

data Either4 a b c d = E4_1 a | E4_2 b | E4_3 c | E4_4 d deriving (Eq, Ord, Show, Generic)

data Either5 a b c d e = E5_1 a | E5_2 b | E5_3 c | E5_4 d | E5_5 e deriving (Eq, Ord, Show, Generic)

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c) => HasAvroSchema (Either3 a b c) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Type) :| [
                             untag (schema :: Tagged b Type),
                             untag (schema :: Tagged c Type)
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d) => HasAvroSchema (Either4 a b c d) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Type) :| [
                             untag (schema :: Tagged b Type),
                             untag (schema :: Tagged c Type),
                             untag (schema :: Tagged d Type)
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e) => HasAvroSchema (Either5 a b c d e) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Type) :| [
                             untag (schema :: Tagged b Type),
                             untag (schema :: Tagged c Type),
                             untag (schema :: Tagged d Type),
                             untag (schema :: Tagged e Type)
                            ])

instance (FromAvro a, FromAvro b, FromAvro c) => FromAvro (Either3 a b c) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E3_1 <$> fromAvro x
    | matches branch schemaB = E3_2 <$> fromAvro x
    | matches branch schemaC = E3_3 <$> fromAvro x
    | otherwise              = badValue e "either3"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
  fromAvro x = badValue x "either3"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d) => FromAvro (Either4 a b c d) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E4_1 <$> fromAvro x
    | matches branch schemaB = E4_2 <$> fromAvro x
    | matches branch schemaC = E4_3 <$> fromAvro x
    | matches branch schemaD = E4_4 <$> fromAvro x
    | otherwise              = badValue e "either4"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
          Tagged schemaD = schema :: Tagged d Type
  fromAvro x = badValue x "either4"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e) => FromAvro (Either5 a b c d e) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E5_1 <$> fromAvro x
    | matches branch schemaB = E5_2 <$> fromAvro x
    | matches branch schemaC = E5_3 <$> fromAvro x
    | matches branch schemaD = E5_4 <$> fromAvro x
    | matches branch schemaE = E5_5 <$> fromAvro x
    | otherwise              = badValue e "either5"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
          Tagged schemaD = schema :: Tagged d Type
          Tagged schemaE = schema :: Tagged e Type
  fromAvro x = badValue x "either5"

instance (ToAvro a, ToAvro b, ToAvro c) => ToAvro (Either3 a b c) where
  toAvro e =
    let sch@(one :| [two, three]) = options (schemaOf e)
    in case e of
      E3_1 a -> T.Union sch one   (toAvro a)
      E3_2 b -> T.Union sch two   (toAvro b)
      E3_3 c -> T.Union sch three (toAvro c)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d) => ToAvro (Either4 a b c d) where
  toAvro e =
    let sch@(one :| [two, three, four]) = options (schemaOf e)
    in case e of
      E4_1 a -> T.Union sch one   (toAvro a)
      E4_2 b -> T.Union sch two   (toAvro b)
      E4_3 c -> T.Union sch three (toAvro c)
      E4_4 d -> T.Union sch four  (toAvro d)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e) => ToAvro (Either5 a b c d e) where
  toAvro e =
    let sch@(one :| [two, three, four, five]) = options (schemaOf e)
    in case e of
      E5_1 a -> T.Union sch one   (toAvro a)
      E5_2 b -> T.Union sch two   (toAvro b)
      E5_3 c -> T.Union sch three (toAvro c)
      E5_4 d -> T.Union sch four  (toAvro d)
      E5_5 e -> T.Union sch five  (toAvro e)
