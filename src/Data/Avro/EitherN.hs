{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Avro.EitherN where

import           Data.Avro
import           Data.Avro.Encoding.FromAvro  (FromAvro (..))
import qualified Data.Avro.Encoding.FromAvro  as AV
import           Data.Avro.Encoding.ToAvro    (ToAvro (..))
import           Data.Avro.Internal.EncodeRaw (putI)
import           Data.Avro.Schema.Schema      as S
import           Data.Bifoldable              (Bifoldable (..))
import           Data.Bifunctor               (Bifunctor (..))
import           Data.Bitraversable           (Bitraversable (..))
import           Data.ByteString.Builder      (Builder)
import           Data.List.NonEmpty
import           Data.Tagged
import qualified Data.Vector                  as V
import           GHC.Generics                 (Generic)

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
  bimap _ _ (E10_1 a)  = E10_1 a
  bimap _ _ (E10_2 a)  = E10_2 a
  bimap _ _ (E10_3 a)  = E10_3 a
  bimap _ _ (E10_4 a)  = E10_4 a
  bimap _ _ (E10_5 a)  = E10_5 a
  bimap _ _ (E10_6 a)  = E10_6 a
  bimap _ _ (E10_7 a)  = E10_7 a
  bimap _ _ (E10_8 a)  = E10_8 a
  bimap f _ (E10_9 a)  = E10_9 (f a)
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
  bifoldMap f _ (E10_9 a)  = f a
  bifoldMap _ g (E10_10 a) = g a
  bifoldMap _ _ _          = mempty

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
  bitraverse f _ (E7_6 a) = E7_6 <$> f a
  bitraverse _ g (E7_7 a) = E7_7 <$> g a

instance Bitraversable (Either8 a b c d e f) where
  bitraverse _ _ (E8_1 a) = pure (E8_1 a)
  bitraverse _ _ (E8_2 a) = pure (E8_2 a)
  bitraverse _ _ (E8_3 a) = pure (E8_3 a)
  bitraverse _ _ (E8_4 a) = pure (E8_4 a)
  bitraverse _ _ (E8_5 a) = pure (E8_5 a)
  bitraverse _ _ (E8_6 a) = pure (E8_6 a)
  bitraverse f _ (E8_7 a) = E8_7 <$> f a
  bitraverse _ g (E8_8 a) = E8_8 <$> g a

instance Bitraversable (Either9 a b c d e f g) where
  bitraverse _ _ (E9_1 a) = pure (E9_1 a)
  bitraverse _ _ (E9_2 a) = pure (E9_2 a)
  bitraverse _ _ (E9_3 a) = pure (E9_3 a)
  bitraverse _ _ (E9_4 a) = pure (E9_4 a)
  bitraverse _ _ (E9_5 a) = pure (E9_5 a)
  bitraverse _ _ (E9_6 a) = pure (E9_6 a)
  bitraverse _ _ (E9_7 a) = pure (E9_7 a)
  bitraverse f _ (E9_8 a) = E9_8 <$> f a
  bitraverse _ g (E9_9 a) = E9_9 <$> g a

instance Bitraversable (Either10 a b c d e f g h) where
  bitraverse _ _ (E10_1 a)  = pure (E10_1 a)
  bitraverse _ _ (E10_2 a)  = pure (E10_2 a)
  bitraverse _ _ (E10_3 a)  = pure (E10_3 a)
  bitraverse _ _ (E10_4 a)  = pure (E10_4 a)
  bitraverse _ _ (E10_5 a)  = pure (E10_5 a)
  bitraverse _ _ (E10_6 a)  = pure (E10_6 a)
  bitraverse _ _ (E10_7 a)  = pure (E10_7 a)
  bitraverse _ _ (E10_8 a)  = pure (E10_8 a)
  bitraverse f _ (E10_9 a)  = E10_9 <$> f a
  bitraverse _ g (E10_10 a) = E10_10 <$> g a

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c) => HasAvroSchema (Either3 a b c) where
  schema = Tagged $ mkUnion (untag @a schema :| [
                             untag @b schema,
                             untag @c schema
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d) => HasAvroSchema (Either4 a b c d) where
  schema = Tagged $ mkUnion (untag @a schema :| [
                             untag @b schema,
                             untag @c schema,
                             untag @d schema
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e) => HasAvroSchema (Either5 a b c d e) where
  schema = Tagged $ mkUnion (untag @a schema :| [
                             untag @b schema,
                             untag @c schema,
                             untag @d schema,
                             untag @e schema
                            ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f)
  => HasAvroSchema (Either6 a b c d e f) where
    schema = Tagged $ mkUnion (untag @a schema :| [
                               untag @b schema,
                               untag @c schema,
                               untag @d schema,
                               untag @e schema,
                               untag @f schema
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g)
  => HasAvroSchema (Either7 a b c d e f g) where
    schema = Tagged $ mkUnion (untag @a schema :| [
                               untag @b schema,
                               untag @c schema,
                               untag @d schema,
                               untag @e schema,
                               untag @f schema,
                               untag @g schema
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g, HasAvroSchema h)
  => HasAvroSchema (Either8 a b c d e f g h) where
    schema = Tagged $ mkUnion (untag @a schema :| [
                               untag @b schema,
                               untag @c schema,
                               untag @d schema,
                               untag @e schema,
                               untag @f schema,
                               untag @g schema,
                               untag @h schema
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g, HasAvroSchema h, HasAvroSchema i)
  => HasAvroSchema (Either9 a b c d e f g h i) where
    schema = Tagged $ mkUnion (untag @a schema :| [
                               untag @b schema,
                               untag @c schema,
                               untag @d schema,
                               untag @e schema,
                               untag @f schema,
                               untag @g schema,
                               untag @h schema,
                               untag @i schema
                              ])

instance (HasAvroSchema a, HasAvroSchema b, HasAvroSchema c, HasAvroSchema d, HasAvroSchema e, HasAvroSchema f, HasAvroSchema g, HasAvroSchema h, HasAvroSchema i, HasAvroSchema j)
  => HasAvroSchema (Either10 a b c d e f g h i j) where
    schema = Tagged $ mkUnion (untag @a schema :| [
                               untag @b schema,
                               untag @c schema,
                               untag @d schema,
                               untag @e schema,
                               untag @f schema,
                               untag @g schema,
                               untag @h schema,
                               untag @i schema,
                               untag @j schema
                              ])

------------ DATA.AVRO.VALUE --------------------------------
instance (FromAvro a, FromAvro b, FromAvro c) => FromAvro (Either3 a b c) where
  fromAvro (AV.Union _ 0 a) = E3_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E3_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E3_3 <$> fromAvro c
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either3 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either3 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d) => FromAvro (Either4 a b c d) where
  fromAvro (AV.Union _ 0 a) = E4_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E4_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E4_3 <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E4_4 <$> fromAvro d
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either4 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either4 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e) => FromAvro (Either5 a b c d e) where
  fromAvro (AV.Union _ 0 a) = E5_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E5_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E5_3 <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E5_4 <$> fromAvro d
  fromAvro (AV.Union _ 4 e) = E5_5 <$> fromAvro e
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either5 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either5 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f) => FromAvro (Either6 a b c d e f) where
  fromAvro (AV.Union _ 0 a) = E6_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E6_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E6_3 <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E6_4 <$> fromAvro d
  fromAvro (AV.Union _ 4 e) = E6_5 <$> fromAvro e
  fromAvro (AV.Union _ 5 f) = E6_6 <$> fromAvro f
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either6 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either6 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g) => FromAvro (Either7 a b c d e f g) where
  fromAvro (AV.Union _ 0 a) = E7_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E7_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E7_3 <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E7_4 <$> fromAvro d
  fromAvro (AV.Union _ 4 e) = E7_5 <$> fromAvro e
  fromAvro (AV.Union _ 5 f) = E7_6 <$> fromAvro f
  fromAvro (AV.Union _ 6 g) = E7_7 <$> fromAvro g
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either7 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either7 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g, FromAvro h) => FromAvro (Either8 a b c d e f g h) where
  fromAvro (AV.Union _ 0 a) = E8_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E8_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E8_3 <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E8_4 <$> fromAvro d
  fromAvro (AV.Union _ 4 e) = E8_5 <$> fromAvro e
  fromAvro (AV.Union _ 5 f) = E8_6 <$> fromAvro f
  fromAvro (AV.Union _ 6 g) = E8_7 <$> fromAvro g
  fromAvro (AV.Union _ 7 h) = E8_8 <$> fromAvro h
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either8 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either8 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g, FromAvro h, FromAvro i) => FromAvro (Either9 a b c d e f g h i) where
  fromAvro (AV.Union _ 0 a) = E9_1 <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E9_2 <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E9_3 <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E9_4 <$> fromAvro d
  fromAvro (AV.Union _ 4 e) = E9_5 <$> fromAvro e
  fromAvro (AV.Union _ 5 f) = E9_6 <$> fromAvro f
  fromAvro (AV.Union _ 6 g) = E9_7 <$> fromAvro g
  fromAvro (AV.Union _ 7 h) = E9_8 <$> fromAvro h
  fromAvro (AV.Union _ 8 i) = E9_9 <$> fromAvro i
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either9 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either9 from a non-union"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e, FromAvro f, FromAvro g, FromAvro h, FromAvro i, FromAvro j) => FromAvro (Either10 a b c d e f g h i j) where
  fromAvro (AV.Union _ 0 a) = E10_1  <$> fromAvro a
  fromAvro (AV.Union _ 1 b) = E10_2  <$> fromAvro b
  fromAvro (AV.Union _ 2 c) = E10_3  <$> fromAvro c
  fromAvro (AV.Union _ 3 d) = E10_4  <$> fromAvro d
  fromAvro (AV.Union _ 4 e) = E10_5  <$> fromAvro e
  fromAvro (AV.Union _ 5 f) = E10_6  <$> fromAvro f
  fromAvro (AV.Union _ 6 g) = E10_7  <$> fromAvro g
  fromAvro (AV.Union _ 7 h) = E10_8  <$> fromAvro h
  fromAvro (AV.Union _ 8 i) = E10_9  <$> fromAvro i
  fromAvro (AV.Union _ 9 j) = E10_10 <$> fromAvro j
  fromAvro (AV.Union _ n _) = Left ("Unable to decode Either10 from a position #" <> show n)
  fromAvro _                = Left "Unable to decode Either10 from a non-union"

putIndexedValue :: ToAvro a => Int -> V.Vector Schema -> a -> Builder
putIndexedValue i opts x = putI i <> toAvro (V.unsafeIndex opts i) x
{-# INLINE putIndexedValue #-}

instance (ToAvro a, ToAvro b, ToAvro c) => ToAvro (Either3 a b c) where
  toAvro (S.Union opts) v =
    if V.length opts == 3
      then case v of
        E3_1 x -> putIndexedValue 0 opts x
        E3_2 x -> putIndexedValue 1 opts x
        E3_3 x -> putIndexedValue 2 opts x
      else error ("Unable to encode Either3 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either3 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d) => ToAvro (Either4 a b c d) where
  toAvro (S.Union opts) v =
    if V.length opts == 4
      then case v of
        E4_1 x -> putIndexedValue 0 opts x
        E4_2 x -> putIndexedValue 1 opts x
        E4_3 x -> putIndexedValue 2 opts x
        E4_4 x -> putIndexedValue 3 opts x
      else error ("Unable to encode Either4 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either4 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e) => ToAvro (Either5 a b c d e) where
  toAvro (S.Union opts) v =
    if V.length opts == 5
      then case v of
        E5_1 x -> putIndexedValue 0 opts x
        E5_2 x -> putIndexedValue 1 opts x
        E5_3 x -> putIndexedValue 2 opts x
        E5_4 x -> putIndexedValue 3 opts x
        E5_5 x -> putIndexedValue 4 opts x
      else error ("Unable to encode Either5 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either5 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f) => ToAvro (Either6 a b c d e f) where
  toAvro (S.Union opts) v =
    if V.length opts == 6
      then case v of
        E6_1 x -> putIndexedValue 0 opts x
        E6_2 x -> putIndexedValue 1 opts x
        E6_3 x -> putIndexedValue 2 opts x
        E6_4 x -> putIndexedValue 3 opts x
        E6_5 x -> putIndexedValue 4 opts x
        E6_6 x -> putIndexedValue 5 opts x
      else error ("Unable to encode Either6 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either6 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g) => ToAvro (Either7 a b c d e f g) where
  toAvro (S.Union opts) v =
    if V.length opts == 7
      then case v of
        E7_1 x -> putIndexedValue 0 opts x
        E7_2 x -> putIndexedValue 1 opts x
        E7_3 x -> putIndexedValue 2 opts x
        E7_4 x -> putIndexedValue 3 opts x
        E7_5 x -> putIndexedValue 4 opts x
        E7_6 x -> putIndexedValue 5 opts x
        E7_7 x -> putIndexedValue 6 opts x
      else error ("Unable to encode Either7 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either7 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g, ToAvro h) => ToAvro (Either8 a b c d e f g h) where
  toAvro (S.Union opts) v =
    if V.length opts == 8
      then case v of
        E8_1 x -> putIndexedValue 0 opts x
        E8_2 x -> putIndexedValue 1 opts x
        E8_3 x -> putIndexedValue 2 opts x
        E8_4 x -> putIndexedValue 3 opts x
        E8_5 x -> putIndexedValue 4 opts x
        E8_6 x -> putIndexedValue 5 opts x
        E8_7 x -> putIndexedValue 6 opts x
        E8_8 x -> putIndexedValue 7 opts x
      else error ("Unable to encode Either8 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either8 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g, ToAvro h, ToAvro i) => ToAvro (Either9 a b c d e f g h i) where
  toAvro (S.Union opts) v =
    if V.length opts == 9
      then case v of
        E9_1 x -> putIndexedValue 0 opts x
        E9_2 x -> putIndexedValue 1 opts x
        E9_3 x -> putIndexedValue 2 opts x
        E9_4 x -> putIndexedValue 3 opts x
        E9_5 x -> putIndexedValue 4 opts x
        E9_6 x -> putIndexedValue 5 opts x
        E9_7 x -> putIndexedValue 6 opts x
        E9_8 x -> putIndexedValue 7 opts x
        E9_9 x -> putIndexedValue 8 opts x
      else error ("Unable to encode Either9 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either9 as " <> show s)

instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d, ToAvro e, ToAvro f, ToAvro g, ToAvro h, ToAvro i, ToAvro j) => ToAvro (Either10 a b c d e f g h i j) where
  toAvro (S.Union opts) v =
    if V.length opts == 10
      then case v of
        E10_1 x  -> putIndexedValue 0 opts x
        E10_2 x  -> putIndexedValue 1 opts x
        E10_3 x  -> putIndexedValue 2 opts x
        E10_4 x  -> putIndexedValue 3 opts x
        E10_5 x  -> putIndexedValue 4 opts x
        E10_6 x  -> putIndexedValue 5 opts x
        E10_7 x  -> putIndexedValue 6 opts x
        E10_8 x  -> putIndexedValue 7 opts x
        E10_9 x  -> putIndexedValue 8 opts x
        E10_10 x -> putIndexedValue 9 opts x
      else  error ("Unable to encode Either10 as " <> show opts)
  toAvro s _ = error ("Unable to encode Either10 as " <> show s)
