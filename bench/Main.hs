{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import qualified Bench.Deconflict as Deconflict
import qualified Bench.Encoding   as Encoding
import qualified Bench.Time       as Time

import Gauge

main :: IO ()
main = defaultMain
  [
    --  Time.encode
  -- , Time.decode

  Deconflict.only
  , Deconflict.notOnly
  , Encoding.encodeToBS
  , Encoding.encodeContainer
  , Encoding.roundtripContainer
  , Deconflict.container
  ]
