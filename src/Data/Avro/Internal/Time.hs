{-# LANGUAGE CPP #-}
module Data.Avro.Internal.Time where

-- Utility functions to work with times

import Data.Maybe            (fromJust)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.Internal
#else
import Data.Time.Format
#endif

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0
{-# INLINE epoch #-}

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

diffTimeToMillis :: DiffTime -> Integer
diffTimeToMillis = (`div` 1000000000) . diffTimeToPicoseconds

millisToDiffTime :: Integer -> DiffTime
millisToDiffTime = picosecondsToDiffTime . (* 1000000000)

utcTimeToMicros :: UTCTime -> Integer
utcTimeToMicros t = diffTimeToPicoseconds (realToFrac (diffUTCTime t epoch)) `div` 1000000

utcTimeToMillis :: UTCTime -> Integer
utcTimeToMillis = (`div` 1000) . utcTimeToMicros

microsToUTCTime :: Integer -> UTCTime
microsToUTCTime x = addUTCTime (realToFrac $ picosecondsToDiffTime (x * 1000000)) epoch

millisToUTCTime :: Integer -> UTCTime
millisToUTCTime x = addUTCTime (realToFrac $ picosecondsToDiffTime (x * 1000000000)) epoch

localTimeToMicros :: LocalTime -> Integer
localTimeToMicros = utcTimeToMicros . localTimeToUTC utc

localTimeToMillis :: LocalTime -> Integer
localTimeToMillis = utcTimeToMillis . localTimeToUTC utc

microsToLocalTime :: Integer -> LocalTime
microsToLocalTime = utcToLocalTime utc . microsToUTCTime

millisToLocalTime :: Integer -> LocalTime
millisToLocalTime = utcToLocalTime utc . millisToUTCTime
