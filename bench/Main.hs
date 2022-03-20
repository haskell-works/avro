{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
import qualified Bench.Deconflict as Deconflict
import qualified Bench.Encoding   as Encoding
import Gauge
#endif

main :: IO ()
main =
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
  defaultMain
    [ Deconflict.values
    , Encoding.encodeToBS
    , Encoding.encodeContainer
    , Encoding.roundtripContainer
    , Deconflict.container
    ]
#else
  return ()
#endif
