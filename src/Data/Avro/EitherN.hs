{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Avro.EitherN where

import           Data.Avro
import           Data.Avro.Decode.Lazy as AL
import           Data.Avro.Schema
import qualified Data.Avro.Types       as T
import           Data.Bifoldable       (Bifoldable (..))
import           Data.Bifunctor        (Bifunctor (..))
import           Data.Bitraversable    (Bitraversable (..))
import           Data.List.NonEmpty
import           Data.Tagged
import           GHC.Generics          (Generic)

data Either3 a b c = E3_1 a | E3_2 b | E3_3 c deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either4 a b c d = E4_1 a | E4_2 b | E4_3 c | E4_4 d deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either5 a b c d e = E5_1 a | E5_2 b | E5_3 c | E5_4 d | E5_5 e deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either6 a b c d e f = E6_1 a | E6_2 b | E6_3 c | E6_4 d | E6_5 e | E6_6 f deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either7 a b c d e f g = E7_1 a | E7_2 b | E7_3 c | E7_4 d | E7_5 e | E7_6 f | E7_7 g deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either8 a b c d e f g h = E8_1 a | E8_2 b | E8_3 c | E8_4 d | E8_5 e | E8_6 f | E8_7 g | E8_8 h deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either9 a b c d e f g h i = E9_1 a | E9_2 b | E9_3 c | E9_4 d | E9_5 e | E9_6 f | E9_7 g | E9_8 h | E9_9 i deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Either10 a b c d e f g h i j = E10_1 a | E10_2 b | E10_3 c | E10_4 d | E10_5 e | E10_6 f | E10_7 g | E10_8 h | E10_9 i | E10_10 j deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Applicative (Either3 a b) where
  pure = E3_3
  E3_1 a <*> _ = E3_1 a
  E3_2 a <*> _ = E3_2 a
  E3_3 f <*> r = fmap f r

instance Applicative (Either4 a b c) where
  pure = E4_4
  E4_1 a <*> _ = E4_1 a
  E4_2 a <*> _ = E4_2 a
  E4_3 a <*> _ = E4_3 a
  E4_4 f <*> r = fmap f r

instance Applicative (Either5 a b c d) where
  pure = E5_5
  E5_1 a <*> _ = E5_1 a
  E5_2 a <*> _ = E5_2 a
  E5_3 a <*> _ = E5_3 a
  E5_4 a <*> _ = E5_4 a
  E5_5 f <*> r = fmap f r

instance Applicative (Either6 a b c d e) where
  pure = E6_6
  E6_1 a <*> _ = E6_1 a
  E6_2 a <*> _ = E6_2 a
  E6_3 a <*> _ = E6_3 a
  E6_4 a <*> _ = E6_4 a
  E6_5 a <*> _ = E6_5 a
  E6_6 f <*> r = fmap f r

instance Applicative (Either7 a b c d e f) where
  pure = E7_7
  E7_1 a <*> _ = E7_1 a
  E7_2 a <*> _ = E7_2 a
  E7_3 a <*> _ = E7_3 a
  E7_4 a <*> _ = E7_4 a
  E7_5 a <*> _ = E7_5 a
  E7_6 a <*> _ = E7_6 a
  E7_7 f <*> r = fmap f r

instance Applicative (Either8 a b c d e f g) where
  pure = E8_8
  E8_1 a <*> _ = E8_1 a
  E8_2 a <*> _ = E8_2 a
  E8_3 a <*> _ = E8_3 a
  E8_4 a <*> _ = E8_4 a
  E8_5 a <*> _ = E8_5 a
  E8_6 a <*> _ = E8_6 a
  E8_7 a <*> _ = E8_7 a
  E8_8 f <*> r = fmap f r

instance Applicative (Either9 a b c d e f g h) where
  pure = E9_9
  E9_1 a <*> _ = E9_1 a
  E9_2 a <*> _ = E9_2 a
  E9_3 a <*> _ = E9_3 a
  E9_4 a <*> _ = E9_4 a
  E9_5 a <*> _ = E9_5 a
  E9_6 a <*> _ = E9_6 a
  E9_7 a <*> _ = E9_7 a
  E9_8 a <*> _ = E9_8 a
  E9_9 f <*> r = fmap f r

instance Applicative (Either10 a b c d e f g h i) where
  pure = E10_10
  E10_1 a <*> _ = E10_1 a
  E10_2 a <*> _ = E10_2 a
  E10_3 a <*> _ = E10_3 a
  E10_4 a <*> _ = E10_4 a
  E10_5 a <*> _ = E10_5 a
  E10_6 a <*> _ = E10_6 a
  E10_7 a <*> _ = E10_7 a
  E10_8 a <*> _ = E10_8 a
  E10_9 a <*> _ = E10_9 a
  E10_10 f <*> r = fmap f r

instance Bifunctor (Either3 a) where
  bimap _ _ (E3_1 a) = E3_1 a
  bimap f _ (E3_2 a) = E3_2 (f a)
  bimap _ g (E3_3 a) = E3_3 (g a)

instance Bifunctor (Either4 a b) where
  bimap _ _ (E4_1 a) = E4_1 a
  bimap _ _ (E4_2 a) = E4_2 a
  bimap f _ (E4_3 a) = E4_3 (f a)
  bimap _ g (E4_4 a) = E4_4 (g a)

instance Bifunctor (Either5 a b c) where
  bimap _ _ (E5_1 a) = E5_1 a
  bimap _ _ (E5_2 a) = E5_2 a
  bimap _ _ (E5_3 a) = E5_3 a
  bimap f _ (E5_4 a) = E5_4 (f a)
  bimap _ g (E5_5 a) = E5_5 (g a)

instance Bifunctor (Either6 a b c d) where
  bimap _ _ (E6_1 a) = E6_1 a
  bimap _ _ (E6_2 a) = E6_2 a
  bimap _ _ (E6_3 a) = E6_3 a
  bimap _ _ (E6_4 a) = E6_4 a
  bimap f _ (E6_5 a) = E6_5 (f a)
  bimap _ g (E6_6 a) = E6_6 (g a)

instance Bifunctor (Either7 a b c d e) where
  bimap _ _ (E7_1 a) = E7_1 a
  bimap _ _ (E7_2 a) = E7_2 a
  bimap _ _ (E7_3 a) = E7_3 a
  bimap _ _ (E7_4 a) = E7_4 a
  bimap _ _ (E7_5 a) = E7_5 a
  bimap f _ (E7_6 a) = E7_6 (f a)
  bimap _ g (E7_7 a) = E7_7 (g a)

instance Bifunctor (Either8 a b c d e f) where
  bimap _ _ (E8_1 a) = E8_1 a
  bimap _ _ (E8_2 a) = E8_2 a
  bimap _ _ (E8_3 a) = E8_3 a
  bimap _ _ (E8_4 a) = E8_4 a
  bimap _ _ (E8_5 a) = E8_5 a
  bimap _ _ (E8_6 a) = E8_6 a
  bimap f _ (E8_7 a) = E8_7 (f a)
  bimap _ g (E8_8 a) = E8_8 (g a)

instance Bifunctor (Either9 a b c d e f g) where
  bimap _ _ (E9_1 a) = E9_1 a
  bimap _ _ (E9_2 a) = E9_2 a
  bimap _ _ (E9_3 a) = E9_3 a
  bimap _ _ (E9_4 a) = E9_4 a
  bimap _ _ (E9_5 a) = E9_5 a
  bimap _ _ (E9_6 a) = E9_6 a
  bimap _ _ (E9_7 a) = E9_7 a
  bimap f _ (E9_8 a) = E9_8 (f a)
  bimap _ g (E9_9 a) = E9_9 (g a)

instance Bifunctor (Either10 a b c d e f g h) where
  bimap _ _ (E10_1 a) = E10_1 a
  bimap _ _ (E10_2 a) = E10_2 a
  bimap _ _ (E10_3 a) = E10_3 a
  bimap _ _ (E10_4 a) = E10_4 a
  bimap _ _ (E10_5 a) = E10_5 a
  bimap _ _ (E10_6 a) = E10_6 a
  bimap _ _ (E10_7 a) = E10_7 a
  bimap _ _ (E10_8 a) = E10_8 a
  bimap f _ (E10_9 a) = E10_9 (f a)
  bimap _ g (E10_10 a) = E10_10 (g a)

instance Monad (Either3 a b) where
  E3_1 a >>= _ = E3_1 a
  E3_2 a >>= _ = E3_2 a
  E3_3 a >>= f = f a

instance Monad (Either4 a b c) where
  E4_1 a >>= _ = E4_1 a
  E4_2 a >>= _ = E4_2 a
  E4_3 a >>= _ = E4_3 a
  E4_4 a >>= f = f a

instance Monad (Either5 a b c d) where
  E5_1 a >>= _ = E5_1 a
  E5_2 a >>= _ = E5_2 a
  E5_3 a >>= _ = E5_3 a
  E5_4 a >>= _ = E5_4 a
  E5_5 a >>= f = f a

instance Monad (Either6 a b c d e) where
  E6_1 a >>= _ = E6_1 a
  E6_2 a >>= _ = E6_2 a
  E6_3 a >>= _ = E6_3 a
  E6_4 a >>= _ = E6_4 a
  E6_5 a >>= _ = E6_5 a
  E6_6 a >>= f = f a

instance Monad (Either7 a b c d e f) where
  E7_1 a >>= _ = E7_1 a
  E7_2 a >>= _ = E7_2 a
  E7_3 a >>= _ = E7_3 a
  E7_4 a >>= _ = E7_4 a
  E7_5 a >>= _ = E7_5 a
  E7_6 a >>= _ = E7_6 a
  E7_7 a >>= f = f a

instance Monad (Either8 a b c d e f g) where
  E8_1 a >>= _ = E8_1 a
  E8_2 a >>= _ = E8_2 a
  E8_3 a >>= _ = E8_3 a
  E8_4 a >>= _ = E8_4 a
  E8_5 a >>= _ = E8_5 a
  E8_6 a >>= _ = E8_6 a
  E8_7 a >>= _ = E8_7 a
  E8_8 a >>= f = f a

instance Monad (Either9 a b c d e f g h) where
  E9_1 a >>= _ = E9_1 a
  E9_2 a >>= _ = E9_2 a
  E9_3 a >>= _ = E9_3 a
  E9_4 a >>= _ = E9_4 a
  E9_5 a >>= _ = E9_5 a
  E9_6 a >>= _ = E9_6 a
  E9_7 a >>= _ = E9_7 a
  E9_8 a >>= _ = E9_8 a
  E9_9 a >>= f = f a

instance Monad (Either10 a b c d e f g h i) where
  E10_1 a >>= _ = E10_1 a
  E10_2 a >>= _ = E10_2 a
  E10_3 a >>= _ = E10_3 a
  E10_4 a >>= _ = E10_4 a
  E10_5 a >>= _ = E10_5 a
  E10_6 a >>= _ = E10_6 a
  E10_7 a >>= _ = E10_7 a
  E10_8 a >>= _ = E10_8 a
  E10_9 a >>= _ = E10_9 a
  E10_10 a >>= f = f a

instance Bifoldable (Either3 a) where
  bifoldMap f _ (E3_2 a) = f a
  bifoldMap _ g (E3_3 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either4 a b) where
  bifoldMap f _ (E4_3 a) = f a
  bifoldMap _ g (E4_4 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either5 a b c) where
  bifoldMap f _ (E5_4 a) = f a
  bifoldMap _ g (E5_5 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either6 a b c d) where
  bifoldMap f _ (E6_5 a) = f a
  bifoldMap _ g (E6_6 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either7 a b c d e) where
  bifoldMap f _ (E7_6 a) = f a
  bifoldMap _ g (E7_7 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either8 a b c d e f) where
  bifoldMap f _ (E8_7 a) = f a
  bifoldMap _ g (E8_8 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either9 a b c d e f g) where
  bifoldMap f _ (E9_8 a) = f a
  bifoldMap _ g (E9_9 a) = g a
  bifoldMap _ _ _        = mempty

instance Bifoldable (Either10 a b c d e f g h) where
  bifoldMap f _ (E10_9 a) = f a
  bifoldMap _ g (E10_10 a) = g a
  bifoldMap _ _ _        = mempty

instance Bitraversable (Either3 a) where
  bitraverse _ _ (E3_1 a) = pure (E3_1 a)
  bitraverse f _ (E3_2 a) = E3_2 <$> f a
  bitraverse _ g (E3_3 a) = E3_3 <$> g a

instance Bitraversable (Either4 a b) where
  bitraverse _ _ (E4_1 a) = pure (E4_1 a)
  bitraverse _ _ (E4_2 a) = pure (E4_2 a)
  bitraverse f _ (E4_3 a) = E4_3 <$> f a
  bitraverse _ g (E4_4 a) = E4_4 <$> g a

instance Bitraversable (Either5 a b c) where
  bitraverse _ _ (E5_1 a) = pure (E5_1 a)
  bitraverse _ _ (E5_2 a) = pure (E5_2 a)
  bitraverse _ _ (E5_3 a) = pure (E5_3 a)
  bitraverse f _ (E5_4 a) = E5_4 <$> f a
  bitraverse _ g (E5_5 a) = E5_5 <$> g a

instance Bitraversable (Either6 a b c d) where
  bitraverse _ _ (E6_1 a) = pure (E6_1 a)
  bitraverse _ _ (E6_2 a) = pure (E6_2 a)
  bitraverse _ _ (E6_3 a) = pure (E6_3 a)
  bitraverse _ _ (E6_4 a) = pure (E6_4 a)
  bitraverse f _ (E6_5 a) = E6_5 <$> f a
  bitraverse _ g (E6_6 a) = E6_6 <$> g a

instance Bitraversable (Either7 a b c d e) where
  bitraverse _ _ (E7_1 a) = pure (E7_1 a)
  bitraverse _ _ (E7_2 a) = pure (E7_2 a)
  bitraverse _ _ (E7_3 a) = pure (E7_3 a)
  bitraverse _ _ (E7_4 a) = pure (E7_4 a)
  bitraverse _ _ (E7_5 a) = pure (E7_5 a)
  bitraverse f _ (E7_6 a) = E7_6 <$> (f a)
  bitraverse _ g (E7_7 a) = E7_7 <$> (g a)

instance Bitraversable (Either8 a b c d e f) where
  bitraverse _ _ (E8_1 a) = pure (E8_1 a)
  bitraverse _ _ (E8_2 a) = pure (E8_2 a)
  bitraverse _ _ (E8_3 a) = pure (E8_3 a)
  bitraverse _ _ (E8_4 a) = pure (E8_4 a)
  bitraverse _ _ (E8_5 a) = pure (E8_5 a)
  bitraverse _ _ (E8_6 a) = pure (E8_6 a)
  bitraverse f _ (E8_7 a) = E8_7 <$> (f a)
  bitraverse _ g (E8_8 a) = E8_8 <$> (g a)

instance Bitraversable (Either9 a b c d e f g) where
  bitraverse _ _ (E9_1 a) = pure (E9_1 a)
  bitraverse _ _ (E9_2 a) = pure (E9_2 a)
  bitraverse _ _ (E9_3 a) = pure (E9_3 a)
  bitraverse _ _ (E9_4 a) = pure (E9_4 a)
  bitraverse _ _ (E9_5 a) = pure (E9_5 a)
  bitraverse _ _ (E9_6 a) = pure (E9_6 a)
  bitraverse _ _ (E9_7 a) = pure (E9_7 a)
  bitraverse f _ (E9_8 a) = E9_8 <$> (f a)
  bitraverse _ g (E9_9 a) = E9_9 <$> (g a)

instance Bitraversable (Either10 a b c d e f g h) where
  bitraverse _ _ (E10_1 a) = pure (E10_1 a)
  bitraverse _ _ (E10_2 a) = pure (E10_2 a)
  bitraverse _ _ (E10_3 a) = pure (E10_3 a)
  bitraverse _ _ (E10_4 a) = pure (E10_4 a)
  bitraverse _ _ (E10_5 a) = pure (E10_5 a)
  bitraverse _ _ (E10_6 a) = pure (E10_6 a)
  bitraverse _ _ (E10_7 a) = pure (E10_7 a)
  bitraverse _ _ (E10_8 a) = pure (E10_8 a)
  bitraverse f _ (E10_9 a) = E10_9 <$> (f a)
  bitraverse _ g (E10_10 a) = E10_10 <$> (g a)

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c) => HasAvroSchema (Either3 a b c) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                             untag (schema :: Tagged b Schema),
                             untag (schema :: Tagged c Schema)
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d) => HasAvroSchema (Either4 a b c d) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                             untag (schema :: Tagged b Schema),
                             untag (schema :: Tagged c Schema),
                             untag (schema :: Tagged d Schema)
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e) => HasAvroSchema (Either5 a b c d e) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                             untag (schema :: Tagged b Schema),
                             untag (schema :: Tagged c Schema),
                             untag (schema :: Tagged d Schema),
                             untag (schema :: Tagged e Schema)
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f)
  => HasAvroSchema (Either6 a b c d e f) where
    schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                               untag (schema :: Tagged b Schema),
                               untag (schema :: Tagged c Schema),
                               untag (schema :: Tagged d Schema),
                               untag (schema :: Tagged e Schema),
                               untag (schema :: Tagged f Schema)
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g)
  => HasAvroSchema (Either7 a b c d e f g) where
    schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                               untag (schema :: Tagged b Schema),
                               untag (schema :: Tagged c Schema),
                               untag (schema :: Tagged d Schema),
                               untag (schema :: Tagged e Schema),
                               untag (schema :: Tagged f Schema),
                               untag (schema :: Tagged g Schema)
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g, HasAvroSchema h)
  => HasAvroSchema (Either8 a b c d e f g h) where
    schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                               untag (schema :: Tagged b Schema),
                               untag (schema :: Tagged c Schema),
                               untag (schema :: Tagged d Schema),
                               untag (schema :: Tagged e Schema),
                               untag (schema :: Tagged f Schema),
                               untag (schema :: Tagged g Schema),
                               untag (schema :: Tagged h Schema)
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g, HasAvroSchema h, HasAvroSchema i)
  => HasAvroSchema (Either9 a b c d e f g h i) where
    schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                               untag (schema :: Tagged b Schema),
                               untag (schema :: Tagged c Schema),
                               untag (schema :: Tagged d Schema),
                               untag (schema :: Tagged e Schema),
                               untag (schema :: Tagged f Schema),
                               untag (schema :: Tagged g Schema),
                               untag (schema :: Tagged h Schema),
                               untag (schema :: Tagged i Schema)
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g, HasAvroSchema h, HasAvroSchema i, HasAvroSchema j)
  => HasAvroSchema (Either10 a b c d e f g h i j) where
    schema = Tagged $ mkUnion (untag (schema :: Tagged a Schema) :| [
                               untag (schema :: Tagged b Schema),
                               untag (schema :: Tagged c Schema),
                               untag (schema :: Tagged d Schema),
                               untag (schema :: Tagged e Schema),
                               untag (schema :: Tagged f Schema),
                               untag (schema :: Tagged g Schema),
                               untag (schema :: Tagged h Schema),
                               untag (schema :: Tagged i Schema),
                               untag (schema :: Tagged j Schema)
                              ])


instance (FromAvro a, FromAvro b, FromAvro c) => FromAvro (Either3 a b c) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E3_1 <$> fromAvro x
    | matches branch schemaB = E3_2 <$> fromAvro x
    | matches branch schemaC = E3_3 <$> fromAvro x
    | otherwise              = badValue e "Either3"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
  fromAvro x = badValue x "Either3"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d) => FromAvro (Either4 a b c d) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E4_1 <$> fromAvro x
    | matches branch schemaB = E4_2 <$> fromAvro x
    | matches branch schemaC = E4_3 <$> fromAvro x
    | matches branch schemaD = E4_4 <$> fromAvro x
    | otherwise              = badValue e "Either4"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
  fromAvro x = badValue x "Either4"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e) => FromAvro (Either5 a b c d e) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E5_1 <$> fromAvro x
    | matches branch schemaB = E5_2 <$> fromAvro x
    | matches branch schemaC = E5_3 <$> fromAvro x
    | matches branch schemaD = E5_4 <$> fromAvro x
    | matches branch schemaE = E5_5 <$> fromAvro x
    | otherwise              = badValue e "Either5"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
  fromAvro x = badValue x "Either5"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f) => FromAvro (Either6 a b c d e f) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E6_1 <$> fromAvro x
    | matches branch schemaB = E6_2 <$> fromAvro x
    | matches branch schemaC = E6_3 <$> fromAvro x
    | matches branch schemaD = E6_4 <$> fromAvro x
    | matches branch schemaE = E6_5 <$> fromAvro x
    | matches branch schemaF = E6_6 <$> fromAvro x
    | otherwise              = badValue e "Either6"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
  fromAvro x = badValue x "Either6"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g) => FromAvro (Either7 a b c d e f g) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E7_1 <$> fromAvro x
    | matches branch schemaB = E7_2 <$> fromAvro x
    | matches branch schemaC = E7_3 <$> fromAvro x
    | matches branch schemaD = E7_4 <$> fromAvro x
    | matches branch schemaE = E7_5 <$> fromAvro x
    | matches branch schemaF = E7_6 <$> fromAvro x
    | matches branch schemaG = E7_7 <$> fromAvro x
      | otherwise              = badValue e "Either7"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
  fromAvro x = badValue x "Either7"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g, FromAvro h) => FromAvro (Either8 a b c d e f g h) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E8_1 <$> fromAvro x
    | matches branch schemaB = E8_2 <$> fromAvro x
    | matches branch schemaC = E8_3 <$> fromAvro x
    | matches branch schemaD = E8_4 <$> fromAvro x
    | matches branch schemaE = E8_5 <$> fromAvro x
    | matches branch schemaF = E8_6 <$> fromAvro x
    | matches branch schemaG = E8_7 <$> fromAvro x
    | matches branch schemaH = E8_8 <$> fromAvro x
    | otherwise              = badValue e "Either8"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
          Tagged schemaH = schema :: Tagged h Schema
  fromAvro x = badValue x "Either8"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g, FromAvro h, FromAvro i) => FromAvro (Either9 a b c d e f g h i) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E9_1 <$> fromAvro x
    | matches branch schemaB = E9_2 <$> fromAvro x
    | matches branch schemaC = E9_3 <$> fromAvro x
    | matches branch schemaD = E9_4 <$> fromAvro x
    | matches branch schemaE = E9_5 <$> fromAvro x
    | matches branch schemaF = E9_6 <$> fromAvro x
    | matches branch schemaG = E9_7 <$> fromAvro x
    | matches branch schemaH = E9_8 <$> fromAvro x
    | matches branch schemaI = E9_9 <$> fromAvro x
    | otherwise              = badValue e "Either9"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
          Tagged schemaH = schema :: Tagged h Schema
          Tagged schemaI = schema :: Tagged i Schema
  fromAvro x = badValue x "Either9"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g, FromAvro h, FromAvro i, FromAvro j) => FromAvro (Either10 a b c d e f g h i j) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E10_1 <$> fromAvro x
    | matches branch schemaB = E10_2 <$> fromAvro x
    | matches branch schemaC = E10_3 <$> fromAvro x
    | matches branch schemaD = E10_4 <$> fromAvro x
    | matches branch schemaE = E10_5 <$> fromAvro x
    | matches branch schemaF = E10_6 <$> fromAvro x
    | matches branch schemaG = E10_7 <$> fromAvro x
    | matches branch schemaH = E10_8 <$> fromAvro x
    | matches branch schemaI = E10_9 <$> fromAvro x
    | matches branch schemaJ = E10_10 <$> fromAvro x
    | otherwise              = badValue e "Either10"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
          Tagged schemaH = schema :: Tagged h Schema
          Tagged schemaI = schema :: Tagged i Schema
          Tagged schemaJ = schema :: Tagged j Schema
  fromAvro x = badValue x "Either10"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c) => FromLazyAvro (Either3 a b c) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E3_1 <$> fromLazyAvro x
    | matches branch schemaB = E3_2 <$> fromLazyAvro x
    | matches branch schemaC = E3_3 <$> fromLazyAvro x
    | otherwise              = badValue e "Either3"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
  fromLazyAvro x = badValue x "Either3"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d) => FromLazyAvro (Either4 a b c d) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E4_1 <$> fromLazyAvro x
    | matches branch schemaB = E4_2 <$> fromLazyAvro x
    | matches branch schemaC = E4_3 <$> fromLazyAvro x
    | matches branch schemaD = E4_4 <$> fromLazyAvro x
    | otherwise              = badValue e "Either4"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
  fromLazyAvro x = badValue x "Either4"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e) => FromLazyAvro (Either5 a b c d e) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E5_1 <$> fromLazyAvro x
    | matches branch schemaB = E5_2 <$> fromLazyAvro x
    | matches branch schemaC = E5_3 <$> fromLazyAvro x
    | matches branch schemaD = E5_4 <$> fromLazyAvro x
    | matches branch schemaE = E5_5 <$> fromLazyAvro x
    | otherwise              = badValue e "Either5"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
  fromLazyAvro x = badValue x "Either5"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e, FromLazyAvro f) => FromLazyAvro (Either6 a b c d e f) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E6_1 <$> fromLazyAvro x
    | matches branch schemaB = E6_2 <$> fromLazyAvro x
    | matches branch schemaC = E6_3 <$> fromLazyAvro x
    | matches branch schemaD = E6_4 <$> fromLazyAvro x
    | matches branch schemaE = E6_5 <$> fromLazyAvro x
    | matches branch schemaF = E6_6 <$> fromLazyAvro x
    | otherwise              = badValue e "Either6"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
  fromLazyAvro x = badValue x "Either6"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e, FromLazyAvro f, FromLazyAvro g) => FromLazyAvro (Either7 a b c d e f g) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E7_1 <$> fromLazyAvro x
    | matches branch schemaB = E7_2 <$> fromLazyAvro x
    | matches branch schemaC = E7_3 <$> fromLazyAvro x
    | matches branch schemaD = E7_4 <$> fromLazyAvro x
    | matches branch schemaE = E7_5 <$> fromLazyAvro x
    | matches branch schemaF = E7_6 <$> fromLazyAvro x
    | matches branch schemaG = E7_7 <$> fromLazyAvro x
      | otherwise              = badValue e "Either7"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
  fromLazyAvro x = badValue x "Either7"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e, FromLazyAvro f, FromLazyAvro g, FromLazyAvro h) => FromLazyAvro (Either8 a b c d e f g h) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E8_1 <$> fromLazyAvro x
    | matches branch schemaB = E8_2 <$> fromLazyAvro x
    | matches branch schemaC = E8_3 <$> fromLazyAvro x
    | matches branch schemaD = E8_4 <$> fromLazyAvro x
    | matches branch schemaE = E8_5 <$> fromLazyAvro x
    | matches branch schemaF = E8_6 <$> fromLazyAvro x
    | matches branch schemaG = E8_7 <$> fromLazyAvro x
    | matches branch schemaH = E8_8 <$> fromLazyAvro x
    | otherwise              = badValue e "Either8"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
          Tagged schemaH = schema :: Tagged h Schema
  fromLazyAvro x = badValue x "Either8"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e, FromLazyAvro f, FromLazyAvro g, FromLazyAvro h, FromLazyAvro i) => FromLazyAvro (Either9 a b c d e f g h i) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E9_1 <$> fromLazyAvro x
    | matches branch schemaB = E9_2 <$> fromLazyAvro x
    | matches branch schemaC = E9_3 <$> fromLazyAvro x
    | matches branch schemaD = E9_4 <$> fromLazyAvro x
    | matches branch schemaE = E9_5 <$> fromLazyAvro x
    | matches branch schemaF = E9_6 <$> fromLazyAvro x
    | matches branch schemaG = E9_7 <$> fromLazyAvro x
    | matches branch schemaH = E9_8 <$> fromLazyAvro x
    | matches branch schemaI = E9_9 <$> fromLazyAvro x
    | otherwise              = badValue e "Either9"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
          Tagged schemaH = schema :: Tagged h Schema
          Tagged schemaI = schema :: Tagged i Schema
  fromLazyAvro x = badValue x "Either9"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e, FromLazyAvro f, FromLazyAvro g, FromLazyAvro h, FromLazyAvro i, FromLazyAvro j) => FromLazyAvro (Either10 a b c d e f g h i j) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E10_1 <$> fromLazyAvro x
    | matches branch schemaB = E10_2 <$> fromLazyAvro x
    | matches branch schemaC = E10_3 <$> fromLazyAvro x
    | matches branch schemaD = E10_4 <$> fromLazyAvro x
    | matches branch schemaE = E10_5 <$> fromLazyAvro x
    | matches branch schemaF = E10_6 <$> fromLazyAvro x
    | matches branch schemaG = E10_7 <$> fromLazyAvro x
    | matches branch schemaH = E10_8 <$> fromLazyAvro x
    | matches branch schemaI = E10_9 <$> fromLazyAvro x
    | matches branch schemaJ = E10_10 <$> fromLazyAvro x
    | otherwise              = badValue e "Either10"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
          Tagged schemaC = schema :: Tagged c Schema
          Tagged schemaD = schema :: Tagged d Schema
          Tagged schemaE = schema :: Tagged e Schema
          Tagged schemaF = schema :: Tagged f Schema
          Tagged schemaG = schema :: Tagged g Schema
          Tagged schemaH = schema :: Tagged h Schema
          Tagged schemaI = schema :: Tagged i Schema
          Tagged schemaJ = schema :: Tagged j Schema
  fromLazyAvro x = badValue x "Either10"

instance (ToAvro a, ToAvro b, ToAvro c) => ToAvro (Either3 a b c) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E3_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E3_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E3_3 c -> T.Union sch (schemaOf c) (toAvro c)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d) => ToAvro (Either4 a b c d) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E4_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E4_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E4_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E4_4 d -> T.Union sch (schemaOf d) (toAvro d)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e) => ToAvro (Either5 a b c d e) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E5_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E5_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E5_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E5_4 d -> T.Union sch (schemaOf d) (toAvro d)
      E5_5 e -> T.Union sch (schemaOf e) (toAvro e)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f) => ToAvro (Either6 a b c d e f) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E6_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E6_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E6_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E6_4 d -> T.Union sch (schemaOf d) (toAvro d)
      E6_5 e -> T.Union sch (schemaOf e) (toAvro e)
      E6_6 f -> T.Union sch (schemaOf f) (toAvro f)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g) => ToAvro (Either7 a b c d e f g) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E7_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E7_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E7_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E7_4 d -> T.Union sch (schemaOf d) (toAvro d)
      E7_5 e -> T.Union sch (schemaOf e) (toAvro e)
      E7_6 f -> T.Union sch (schemaOf f) (toAvro f)
      E7_7 g -> T.Union sch (schemaOf g) (toAvro g)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g, ToAvro h) => ToAvro (Either8 a b c d e f g h) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E8_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E8_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E8_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E8_4 d -> T.Union sch (schemaOf d) (toAvro d)
      E8_5 e -> T.Union sch (schemaOf e) (toAvro e)
      E8_6 f -> T.Union sch (schemaOf f) (toAvro f)
      E8_7 g -> T.Union sch (schemaOf g) (toAvro g)
      E8_8 h -> T.Union sch (schemaOf h) (toAvro h)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g, ToAvro h, ToAvro i) => ToAvro (Either9 a b c d e f g h i) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E9_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E9_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E9_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E9_4 d -> T.Union sch (schemaOf d) (toAvro d)
      E9_5 e -> T.Union sch (schemaOf e) (toAvro e)
      E9_6 f -> T.Union sch (schemaOf f) (toAvro f)
      E9_7 g -> T.Union sch (schemaOf g) (toAvro g)
      E9_8 h -> T.Union sch (schemaOf h) (toAvro h)
      E9_9 i -> T.Union sch (schemaOf i) (toAvro i)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g, ToAvro h, ToAvro i, ToAvro j) => ToAvro (Either10 a b c d e f g h i j) where
  toAvro e =
    let sch = options (schemaOf e)
    in case e of
      E10_1 a -> T.Union sch (schemaOf a) (toAvro a)
      E10_2 b -> T.Union sch (schemaOf b) (toAvro b)
      E10_3 c -> T.Union sch (schemaOf c) (toAvro c)
      E10_4 d -> T.Union sch (schemaOf d) (toAvro d)
      E10_5 e -> T.Union sch (schemaOf e) (toAvro e)
      E10_6 f -> T.Union sch (schemaOf f) (toAvro f)
      E10_7 g -> T.Union sch (schemaOf g) (toAvro g)
      E10_8 h -> T.Union sch (schemaOf h) (toAvro h)
      E10_9 i -> T.Union sch (schemaOf i) (toAvro i)
      E10_10 j -> T.Union sch (schemaOf j) (toAvro j)
