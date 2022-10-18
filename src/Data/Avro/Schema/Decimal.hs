{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.Avro.Schema.Decimal
( Decimal
, fromUnderlyingValue
, underlyingValue )
where

import qualified Data.BigDecimal as D
import           Data.Proxy
import           GHC.TypeLits

newtype Decimal (p :: Nat) (s :: Nat)
  = Decimal { unDecimal :: D.BigDecimal }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real)

intScale :: D.BigDecimal -> Integer
intScale = toInteger . D.scale

fromUnderlyingValue
  :: forall p s. KnownNat s
  => Integer -> Decimal p s
fromUnderlyingValue n
  = Decimal $ D.BigDecimal n (fromIntegral $ natVal (Proxy :: Proxy s))

underlyingValue
  :: forall s p. (KnownNat p, KnownNat s)
  => Decimal p s -> Maybe Int
underlyingValue (Decimal d)
  = let ss = natVal (Proxy :: Proxy s)
        pp = natVal (Proxy :: Proxy p)
        new = if ss > intScale d
                 then D.BigDecimal (D.value d * 10 ^ (ss - intScale d)) (fromIntegral ss)
                 else D.roundBD d (D.halfUp (fromIntegral ss))
    in if D.precision new > fromIntegral pp
          then Nothing
          else Just $ fromInteger $ D.value new
