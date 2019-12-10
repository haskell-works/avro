{-# LANGUAGE CPP #-}

#if MIN_VERSION_GLASGOW_HASKELL(8,4,4,0)
{-# OPTIONS_GHC -F -pgmF doctest-discover #-}
#else
module Main where

import qualified System.IO as IO

main :: IO ()
main = IO.putStrLn "WARNING: doctest will not run on GHC versions earlier than 8.4.4"
#endif
