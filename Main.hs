
module Main where

import Prelude hiding (sum)
import Stream
import CoIteration

run :: CStream t s -> Stream t
run (Co tx sx) = let (vx, sx') = tx sx 
                 in  vx : run (Co tx sx')
                     
-- 
prVal :: Show t => CoIteration.Val t -> String
prVal CoIteration.E = "E"
prVal (CoIteration.V t) = show t
                     
prPairVal (a,b) = (prVal a, prVal b)

-- Utilities

base_clock :: CStream Bool Bool
base_clock = Co (\s -> (True,s)) True

val_plus (CoIteration.V x) (CoIteration.V y) = CoIteration.V $ x + y
val_div (CoIteration.V x) (CoIteration.V y) = CoIteration.V $ x `div` y

-- Examples
constof :: t -> CStream (CoIteration.Val t) Bool
constof n = co_const n base_clock

-- E.g., take 20 $ map prVal $ run $ constof 1

average :: CStream (CoIteration.Val Int) s1 -> CStream (CoIteration.Val Int) s2 -> CStream (CoIteration.Val Int) ((s1,s2), Bool)
average x y = co_op val_div (co_op val_plus x y) (constof 2)

-- E.g., take 20 $ map prVal $ run $ average (constof 10) (constof 15)

val_xor (CoIteration.V a) (CoIteration.V b) = 
  CoIteration.V (a && not b || not a && b)
val_and (CoIteration.V a) (CoIteration.V b) = 
  CoIteration.V (a && b)
val_or (CoIteration.V a) (CoIteration.V b) = 
  CoIteration.V (a || b)
val_not (CoIteration.V a) = (CoIteration.V (not a))
  
full_adder
  :: CStream (CoIteration.Val Bool) s1
     -> CStream (CoIteration.Val Bool) s2
     -> CStream (CoIteration.Val Bool) s3
     -> CStream
          (CoIteration.Val Bool, CoIteration.Val Bool)
          (((s1, s2), s3), (((s1, s2), (s2, s3)), (s1, s3)))
full_adder a b c = co_product s cn
  where
    s = co_op val_xor (co_op val_xor a b) c
    cn= co_op val_or 
         (co_op val_or 
          (co_op val_and a b) 
          (co_op val_and b c)) 
         (co_op val_and a c)
         
-- E.g., take 20 $ map prPairVal $ run $ full_adder (constof True) (constof True) (constof True)

         
half_adder
  :: CStream (CoIteration.Val Bool) s1
     -> CStream (CoIteration.Val Bool) s2
     -> CStream
          (CoIteration.Val Bool, CoIteration.Val Bool) ((s1, s2), (s1, s2))
half_adder a b = co_product s c
  where
    s = co_op val_xor a b
    c = co_op val_and a b
    
full_adder1
  :: CStream (CoIteration.Val Bool) s1
     -> CStream (CoIteration.Val Bool) s2
     -> CStream (CoIteration.Val Bool) s3
     -> CStream
          (CoIteration.Val Bool, CoIteration.Val Bool)
          (((s3, ((s1, s2), (s1, s2))), (s3, ((s1, s2), (s1, s2)))),
           (((s1, s2), (s1, s2)),
            ((s3, ((s1, s2), (s1, s2))), (s3, ((s1, s2), (s1, s2))))))
full_adder1 a b c = co_product s cn
  where
    sc1 = half_adder a b
    s1  = co_fst sc1
    c1  = co_snd sc1
      
    sc2 = half_adder c s1
    s   = co_fst sc2
    c2  = co_snd sc2

    cn  = co_op val_or c1 c2


genpreex :: CStream (CoIteration.Val Int) (Int, Bool, Bool)
genpreex = 
  let clk = base_clock 
  in co_genpre 
     (co_const 1 clk) 
     (co_const 2 clk) 
     clk

-- E.g., take 20 $ map prVal $ run $ genpreex

-- from :: Int -> CStream (CoIteration.Val Int) (CoIteration.Val Int)
-- from m = Co (\s -> (s, val_plus s (CoIteration.V 1))) (CoIteration.V m)
  -- let nat clk = co_genpre
  --               (m clk)
  --               (co_op val_plus
  --                (co_pre Nil (nat clk) clk)
  --                (co_const 1 clk)) clk
  -- in co_rec (nat base_clock)
         
from (Co cl icl) m =         
  Co (\(k,scl) ->
        case cl scl of
          (False, scl') ->
            (CoIteration.E, (val_plus k (CoIteration.V 1),scl'))
          (True, scl') ->
            let v = val_plus k (CoIteration.V 1) in
            (v, (v,scl')))
  (CoIteration.V m, icl)
  
-- E.g., take 20 $ map prVal $ run $ from base_clock 0

edge :: CStream (CoIteration.Val Bool) s1
        -> CStream (CoIteration.Val Bool) (s1, (State Bool, Bool, s1))

edge c = co_op val_and c
         (co_unaryop val_not
          (co_fby (co_const False base_clock) c))

edgedetector = 
  let clk = Co (\s -> if s == 0 then (CoIteration.V False, 1)
                      else if s == 1 then (CoIteration.V False, 2) 
                           else (CoIteration.V True, 0)) 0
  in edge clk
             
-- E.g., take 20 $ map prVal $ run $ edgedetector


xwhenc = co_when x c
  where
    x = Co (\s -> if s == 0 then (CoIteration.V 1, 1) 
                  else if s == 1 then (CoIteration.V 2, 2) 
                       else (CoIteration.V 0, 0)) 0
    c = Co (\s -> if s == 0 then (CoIteration.V False, 1) 
                  else (CoIteration.V True, 0)) 0
        
-- E.g., take 20 $ map prVal $ run $ xwhenc        
    
trueonc = on base_clock c
  where
    c = Co (\s -> if s == 0 then (CoIteration.V False, 1) 
                  else (CoIteration.V True, 0)) 0

-- E.g., take 20 $ map show $ run $ trueonc
    
sum (Co cl icl) (Co f s0) = 
  Co (\(acc,s,scl) -> 
        case cl scl of
          (CoIteration.E, scl')  -> 
            let (next, s') = f s 
            in  (CoIteration.E, (acc,s',scl'))
          (CoIteration.V False, scl') -> 
            let (next, s') = f s 
            in  (CoIteration.E, (acc,s',scl'))
          (CoIteration.V True,  scl') -> 
            let (next, s') = f s
                next' = val_plus acc next
            in  (next', (next',s',scl'))) 
   (CoIteration.V 0, s0, icl)
  
-- E.g., take 20 $ map prVal $ run $ sum (sample 1) (from base_clock 0)

sampled_sum x y = sum y (co_when x y)

sample n = 
  Co (\i -> let i' = if i==n-1 then 0 
                     else i+1
                b' = i' == 0 
            in  (CoIteration.V b', i')) 0
  
sumten dx = sampled_sum dx ten  
  where
    ten = sample 10
    
-- E.g., take 100 $ map prVal $ run $ sumten (from base_clock 0)
    
    
-- [1,2,3,4]
-- [3,5,7,4]
-- [10,9,7,4]
-- [10,9,7,4]
-- ...

-- [1,3,10,10,...] -> stream1
-- [2,5,9,9,...] -> stream2
-- [3,7,7,7,...] -> stream3
-- [4,4,4,4,...] -> stream4

-- s1 = (V 1,V s2)
-- s2 = (V 2,V s3)
-- s3 = (V 3,V s4)
-- s4 = (V 4,E)

-- co_prefix i chum (Co tc ic) h =
--   Co (\(CoIteration.V i, chum, s) ->
--        case tc s of
--          (True,  s') -> case chum of
--            Co fchum (CoIteration.V j, chumchum, _) -> 
--              let v = CoIteration.V (i+j) in
--              (v, (v, chumchum, s'))
--          (False, s') -> (CoIteration.E, (CoIteration.V i, chum, s'))) 
--   (CoIteration.V i, chum, ic)
  
{-

- 옆 노드 chum을 현재 노드의 state나 value로 가리킬 수 있을까?
- chum의 값을 읽어 새로운 값을 구해내는 계산을 오직 Co 정의를 통해서만 가능한가? high-level primitive를 사용해서 표현하지 못하는가?
- 

-}

