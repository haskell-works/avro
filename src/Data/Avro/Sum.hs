{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# options -Wall -fno-warn-unticked-promoted-constructors #-}

module Data.Avro.Sum where

import           Data.Avro
import           Data.Avro.Encoding.FromAvro  (FromAvro (..))
import qualified Data.Avro.Encoding.FromAvro  as AV
import           Data.Avro.Encoding.ToAvro    (ToAvro (..))
import           Data.Avro.Internal.EncodeRaw (putI)
import           Data.Avro.Schema.Schema      as S
import           Data.ByteString.Builder      (Builder)
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Tagged                  (Tagged (..))
import qualified Data.Vector                  as V
import           Data.Kind                    (Type, Constraint)
import           Data.Proxy                   (Proxy (..))
import           GHC.TypeLits                 (KnownNat, natVal, Nat, type (+))

-- | N-ary sum type. Used for modelling unions.
data NSum :: [Type] -> Type where
  -- | The contained element is the first one in the list of possibilities.
  --   It is "at the start" of the list.
  Start :: x -> NSum (x ': xs)
  -- | Add a new possibility in the list of types.
  Next :: NSum xs -> NSum (x ': xs)

type family All (c :: Type -> Constraint) (xs :: [Type]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

instance (All Eq xs) => Eq (NSum xs) where
  Start x == Start y = x == y
  Next  x == Next  y = x == y
  _       == _       = False

instance Show (NSum '[]) where
  show = \case

instance (Show x, Show (NSum xs)) => Show (NSum (x : xs)) where
  showsPrec d ns = case ns of
    Start x   -> showParen (d > precedence) $ showString "Start " . showsPrec (precedence + 1) x
    Next next -> showParen (d > precedence) $ showString "Next "  . showsPrec (precedence + 1) next

    where precedence = 10


----------------------------- Construction helper ------------------------------

makeNSum :: IsElem t ts => t -> NSum ts
makeNSum = makeNSumExplicit elementProof

makeNSumExplicit :: Elem t ts -> t -> NSum ts
makeNSumExplicit Here       x = Start x
makeNSumExplicit (There el) x = Next (makeNSumExplicit el x)

-- | @Elem x xs@ is a proof that x is an element of the list xs.
data Elem :: Type -> [Type] -> Type where
  Here :: Elem t (t : ts)
  There :: Elem t ts -> Elem t (y : ts)

class IsElem (t :: Type) (ts :: [Type]) where
  elementProof :: Elem t ts

instance IsElem t (t : ts) where
  elementProof = Here

instance {-# OVERLAPPABLE #-} IsElem t ts => IsElem t (x : ts) where
  elementProof = There elementProof


------------------------ ToAvro ------------------------


instance (All ToAvro xs, Length xs ~ n, KnownNat n) => ToAvro (NSum xs) where
  toAvro :: Schema -> NSum xs -> Builder
  toAvro (S.Union opts) ns =
    if nsLength ns == V.length opts
      then putI index <> avroBuilder ns (V.unsafeIndex opts index)
      else error $ "Unable to encode NSum as the union of " <> show opts
    where
      index = nsIndex ns

      avroBuilder :: All ToAvro ts => NSum ts -> Schema -> Builder
      avroBuilder = \case
        Start x -> flip toAvro x
        Next n  -> avroBuilder n

  toAvro s _ns = error $ "Unable to encode an NSum as " <> show s


type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (x : xs) = 1 + (Length xs)

-- | The number of available types in the NSum
nsLength :: forall xs n. (Length xs ~ n, KnownNat n) => NSum xs -> Int
nsLength _ = fromIntegral $ natVal (Proxy @n)

-- | The index of the NSum's value among the type list
nsIndex :: NSum ts -> Int
nsIndex = \case
  Start _  -> 0
  Next ns -> succ (nsIndex ns)


------------------------ FromAvro ------------------------

instance (All FromAvro xs, KnownListShape xs) => FromAvro (NSum xs) where
  fromAvro :: AV.Value -> Either String (NSum xs)
  fromAvro = \case
    AV.Union _ i val -> runParsers (parsersFromAvro theShape) i val
    _ -> Left $ "Unable to decode an NSum: Not a Union value"

runParsers :: Parsers xs -> Int -> AV.Value -> Either String (NSum xs)
runParsers parsers i val = case parsers of
  PZero -> Left "No choices"
  PNext parser ps -> case compare i 0 of
    LT -> Left "Out of bounds"
    EQ -> Start <$> parser val
    GT -> Next <$> runParsers ps (pred i) val

-- | If all the xs have FromAvro, then we can have the vector (of length xs) of all its parsers.
--   The need for the 'Shape' singleton witness is a technicality: It gives no info
--   but it reflects the type-level data about the list to the value level.
--   This argument will be magicked away with a type class.
parsersFromAvro :: All FromAvro xs => ListShape xs -> Parsers xs
parsersFromAvro = \case
  ShapeZ   -> PZero
  ShapeS s -> PNext fromAvro $ parsersFromAvro s

-- | The function type needed to satisfy the 'FromAvro' interface
type Parser a = AV.Value -> Either String a

-- | A parser for each element of the type list
data Parsers :: [Type] -> Type where
  PZero :: Parsers '[]
  PNext :: Parser x -> Parsers xs -> Parsers (x : xs)

-- | A value-level witness of the structure of the type-level list
--   Enables us to pattern match on it.
--   See https://blog.jle.im/entry/introduction-to-singletons-1.html
data ListShape :: [k] -> Type where
  ShapeZ :: ListShape '[]
  ShapeS :: ListShape xs -> ListShape (x : xs)

-- | Automates the delivery of the 'ListShape' singleton.
--   Inhabited by every possible list.
class KnownListShape (xs :: [k]) where
  theShape :: ListShape xs

instance KnownListShape '[] where
  theShape = ShapeZ

instance KnownListShape xs => KnownListShape (x : xs) where
  theShape = ShapeS theShape


----------------------------- HasAvroSchema ------------------------------

instance forall x xs. (All HasAvroSchema (x : xs), KnownListShape xs) => HasAvroSchema (NSum (x : xs)) where
  schema = Tagged $ mkUnion $
    unTagged (schema @x) :| schemasToList (theSchemas @xs)

theSchemas :: (All HasAvroSchema xs, KnownListShape xs) => Schemas xs
theSchemas = schemasFromShape theShape

-- | The set of Tagged Schemas that match the list of types
--   Use of this type helps ensure the correctness of the HasAvroSchema instance.
data Schemas :: [Type] -> Type where
  SchemaZ :: Schemas '[]
  SchemaS :: Tagged x Schema -> Schemas xs -> Schemas (x : xs)

schemasToList :: Schemas xs -> [Schema]
schemasToList = \case
  SchemaZ               -> []
  SchemaS (Tagged s) ss -> s : schemasToList ss

schemasFromShape :: All HasAvroSchema xs => ListShape xs -> Schemas xs
schemasFromShape = \case
  ShapeZ   -> SchemaZ
  ShapeS s -> SchemaS schema $ schemasFromShape s


