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
    | otherwise              = badValue e "Either3"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
  fromAvro x = badValue x "Either3"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d) => FromAvro (Either4 a b c d) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E4_1 <$> fromAvro x
    | matches branch schemaB = E4_2 <$> fromAvro x
    | matches branch schemaC = E4_3 <$> fromAvro x
    | matches branch schemaD = E4_4 <$> fromAvro x
    | otherwise              = badValue e "Either4"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
          Tagged schemaD = schema :: Tagged d Type
  fromAvro x = badValue x "Either4"

instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d, FromAvro e) => FromAvro (Either5 a b c d e) where
  fromAvro e@(T.Union _ branch x)
    | matches branch schemaA = E5_1 <$> fromAvro x
    | matches branch schemaB = E5_2 <$> fromAvro x
    | matches branch schemaC = E5_3 <$> fromAvro x
    | matches branch schemaD = E5_4 <$> fromAvro x
    | matches branch schemaE = E5_5 <$> fromAvro x
    | otherwise              = badValue e "Either5"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
          Tagged schemaD = schema :: Tagged d Type
          Tagged schemaE = schema :: Tagged e Type
  fromAvro x = badValue x "Either5"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c) => FromLazyAvro (Either3 a b c) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E3_1 <$> fromLazyAvro x
    | matches branch schemaB = E3_2 <$> fromLazyAvro x
    | matches branch schemaC = E3_3 <$> fromLazyAvro x
    | otherwise              = badValue e "Either3"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
  fromLazyAvro x = badValue x "Either3"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d) => FromLazyAvro (Either4 a b c d) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E4_1 <$> fromLazyAvro x
    | matches branch schemaB = E4_2 <$> fromLazyAvro x
    | matches branch schemaC = E4_3 <$> fromLazyAvro x
    | matches branch schemaD = E4_4 <$> fromLazyAvro x
    | otherwise              = badValue e "Either4"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
          Tagged schemaD = schema :: Tagged d Type
  fromLazyAvro x = badValue x "Either4"

instance (FromLazyAvro a, FromLazyAvro b, FromLazyAvro c, FromLazyAvro d, FromLazyAvro e) => FromLazyAvro (Either5 a b c d e) where
  fromLazyAvro e@(AL.Union _ branch x)
    | matches branch schemaA = E5_1 <$> fromLazyAvro x
    | matches branch schemaB = E5_2 <$> fromLazyAvro x
    | matches branch schemaC = E5_3 <$> fromLazyAvro x
    | matches branch schemaD = E5_4 <$> fromLazyAvro x
    | matches branch schemaE = E5_5 <$> fromLazyAvro x
    | otherwise              = badValue e "Either5"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
          Tagged schemaC = schema :: Tagged c Type
          Tagged schemaD = schema :: Tagged d Type
          Tagged schemaE = schema :: Tagged e Type
  fromLazyAvro x = badValue x "Either5"

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
