{-# LANGUAGE BangPatterns #-}

{-
Copyright 2018 mniip

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import qualified Data.Map                      as M
import           Data.HashTable.Class          as HC
import qualified Data.HashTable.ST.Basic       as HTST
import           Data.Hashable
import qualified Data.Set                      as S
import           Data.List
import           Control.Exception
import           Control.Monad.ST
import           Control.Arrow

slist :: [Int]
slist = tail $ go 290797 where go s = s : go (s * s `mod` 50515093)

tlist :: [Int]
tlist = map (`mod` 2000) slist

points :: [(Int, Int)]
points = go tlist where go (x : y : t) = (x, y) : go t

type Ratio = (Int, Int)

reduce :: Ratio -> Ratio
reduce r@(0, 0) = r
reduce (  0, y) = (0, 1)
reduce (  x, 0) = (1, 0)
reduce (  x, y) = let g = signum x * gcd x y in (x `div` g, y `div` g)

pairs :: (a -> a -> b) -> [a] -> [b]
pairs f ls = do
  x : xs <- tails ls
  f x <$> xs

type MyMap s = HTST.HashTable s (Ratio) (S.Set Ratio)

mkLines :: [(Int, Int)] -> ST s (MyMap s)
mkLines pts =
  fromListWith S.union . map (\(x, y) -> (x, S.singleton y)) $ pairs myslope pts
 where
  myslope (x1, y1) (x2, y2) =
    let dx = x2 - x1
    in  if dx == 0
          then ((0, 1), (1, x1))
          else
            let (x, y) = reduce (dx, y2 - y1)
            in  ((x, y), reduce (x, y1 * x - x1 * y))


interCount :: MyMap s -> ST s Int
interCount m = do
  elems <- hElems m
  pure $ sumprod (map S.size elems)
 where
  sumprod xs = let grs_sum = sum xs in sum $ map (\l -> l * (grs_sum - l)) xs

main =
  let c = runST $ do
        lines <- mkLines $ take 2500 points
        interCount lines
  in  print $ show c


-- | Create a hash table from a list of key-value pairs. /O(n)/.
fromListWith :: (HashTable h, Eq k, Hashable k)
             => (v -> v -> v)
             -> [(k, v)]
             -> ST s (h s k v)
fromListWith combine l = do
  ht <- new
  go ht l

 where
  go ht = go'
   where
    go' []              = return ht
    go' ((!k, !v) : xs) = do
      HC.mutate ht k (\mv -> case mv of
        Just old_v -> (Just $ combine old_v v, ())
        Nothing -> (Just v, ()))
      go' xs
{-# INLINE fromListWith #-}

hElems :: (HashTable h, Eq k, Hashable k) => h s k v -> ST s [v]
hElems = foldM (\a (k, v) -> pure (v : a)) []
{-# INLINE hElems #-}
