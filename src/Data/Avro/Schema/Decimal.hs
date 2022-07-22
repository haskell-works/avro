{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.Avro.Schema.Decimal where

import qualified Data.BigDecimal as D
import           Data.Scientific as S
import           Data.Proxy
import           GHC.TypeLits

newtype Decimal (precision :: Nat) (scale :: Nat)
  = Decimal { unDecimal :: D.BigDecimal }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real)

fromUnderlyingValue
  :: forall p s. KnownNat s
  => Integer -> Decimal p s
fromUnderlyingValue n
  = Decimal $ D.BigDecimal n (natVal (Proxy :: Proxy s))

underlyingValue
  :: forall s p. (KnownNat p, KnownNat s)
  => Decimal p s -> Maybe Int
underlyingValue (Decimal d)
  = let ss = natVal (Proxy :: Proxy s)
        pp = natVal (Proxy :: Proxy p)
        new = if ss > D.getScale d
                 then D.BigDecimal (D.getValue d * 10 ^ (ss - D.getScale d)) ss
                 else D.roundBD d (D.halfUp ss)
    in if D.precision new > pp
          then Nothing
          else Just $ fromInteger $ D.getValue new

clamp :: forall p s. (KnownNat p, KnownNat s) => Decimal p s -> Decimal p s
clamp (Decimal d)
  = let ss = natVal (Proxy :: Proxy s)
        pp = natVal (Proxy :: Proxy p)
        new = if ss > D.getScale d
                 then D.BigDecimal (D.getValue d * 10 ^ (ss - D.getScale d)) ss
                 else D.roundBD d (D.halfUp ss)
    in Decimal new

toScientific :: (KnownNat p, KnownNat s) => Decimal p s -> Scientific
toScientific d 
  = let (Decimal d') = clamp d
    in S.scientific (D.getValue d') (negate $ fromIntegral $ D.getScale d')
