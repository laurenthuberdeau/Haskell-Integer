{-# LANGUAGE MagicHash #-}

module Main where

import Prelude hiding (sum)
import Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, defaultMain, nf)
import GHC.Exts


-- fib :: Integral a => a -> a
-- fib n | n < 2  = n
--       | True   = fib (n - 1) + fib (n - 2)

fibInt :: Int -> Int
fibInt n | n < 2 = n
         | True  = fibInt (n - 1) + fibInt (n - 2)

fibUnboxedInt :: Int -> Int
fibUnboxedInt (I# i) = I# (go i)
  where
    go :: Int# -> Int#
    go n | (isTrue# (n <# 2#)) = n
         | True                = go (n -# 1#) +# go (n -# 2#)

-- fibInteger :: Integer -> Integer
-- fibInteger n | n < 2 = n
--              | True  = fibInteger (n - 1) + fibInteger (n - 2)

-- sum :: Integral a => a -> a -> a
-- sum acc 0 = acc
-- sum acc n = sum (acc + n) (n - 1)

main :: IO ()
main = defaultMain
  [ bgroup "fib"
    [ -- bench "Int"     $ nf fib (40 :: Int)
--     , bench "Integer" $ nf fib (40 :: Integer)
      bench "Int (Specialized)"     $ nf fibInt 40
--     , bench "Integer (Specialized)" $ nf fibInteger 40
    , bench "Unboxed Int" $ nf fibUnboxedInt 40
    ]
--   , bgroup "sum"
--     [ bench "Int"     $ nf (sum 0) (1000000 :: Int)
--     , bench "Integer" $ nf (sum 0) (1000000 :: Integer)
--     , bench "Integer (Large)" $ nf (sum (fromIntegral ((maxBound :: Int) + 1))) (1000000 :: Integer)
--     ]
  ]

-- main = pure ()