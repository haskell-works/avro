{-# LANGUAGE CPP #-}
module Data.Avro.Types.Time where

-- Utility functions to work with times

import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Clock
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.Internal
#else
import Data.Time.Format
#endif

epochDate :: Day
epochDate = fromJust $ buildTime defaultTimeLocale []

daysSinceEpoch :: Day -> Integer
daysSinceEpoch d = diffDays d epochDate

fromDaysSinceEpoch :: Integer -> Day
fromDaysSinceEpoch n = addDays n epochDate

diffTimeToMicros :: DiffTime -> Integer
diffTimeToMicros = (`div` 1000000) . diffTimeToPicoseconds

microsToDiffTime :: Integer -> DiffTime
microsToDiffTime = picosecondsToDiffTime . (* 1000000)