{-# OPTIONS_GHC -Wno-deprecations #-}

module Utils (randomNum) where

import System.IO.Unsafe
import System.Random

randomNum :: Int -> Int -> Int
{-# NOINLINE randomNum #-}
randomNum min max = unsafePerformIO (getStdRandom (randomR (min, max)))

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound s = (i `mod` bound, s') where (i, s') = next s

randomNum1 :: Int -> Int
randomNum1 x = fst (nextBounded x (mkStdGen x))
