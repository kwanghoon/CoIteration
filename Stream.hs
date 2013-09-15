
module Stream where

import Prelude hiding (even, const)

-- data Stream t = t : Stream t
type Stream t = [t]

pre :: t -> Stream t -> Stream t
-- pre v (x : xs) = v : pre x xs   -- The origintal definition is too strict.
pre v xs = v : pre (head xs) (tail xs)

plus :: Stream Int -> Stream Int -> Stream Int
plus (x:xs) (y:ys) = x+y : plus xs ys

even :: Stream t -> Stream t
even (x:y:xs) = x : even xs

const :: t -> Stream t
const v = v : const v

extend :: Stream (t1 -> t2) -> Stream t1 -> Stream t2
extend (f:fs) (e:es) = f e : extend fs es

co_plus1 x y = extend (extend (const (+)) x) y  

-- Equivalent to plus
-- E.g., take 20 $ co_plus1 [1..] [11..]

(x:xs) `fby` ys = x : ys

-- Generalize pre
-- E.g., pre v xs = fby (v:zs) xs for an arbitrary list zs

nat = pre 0 (plus nat (const 1))

-- Clocked Streams
data F t s = P t s | S s

data Val t = E | V t

clock s n = case s n of
  E -> False
  V v -> True
  
  
--

merge (False:cs) xs (y:ys) = y : (merge cs xs ys)
merge (True:cs) (x:xs) ys = x : (merge cs xs ys)
