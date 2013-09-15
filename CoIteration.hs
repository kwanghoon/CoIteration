
module CoIteration where

type F t s = (t, s)
data CStream t s = Co (s -> F t s) s

{-
co_pre :: t -> CStream t s -> CStream t (t, s)
co_pre v (Co tx ix) = 
  Co (\(s,sx) -> let (v1, sx1) = tx sx
                 in  (s, (v1,sx1))) (v,ix)
-}

co_pre :: t -> CStream (Val t) s -> CStream Bool s' 
          -> CStream (Val t) (t,s,s')
co_pre v (Co e ie) (Co cl icl) =
  Co (\(pre,se,scl) -> case cl scl of 
         (False, scl') -> 
           (E, let (E, se') = e se in (pre,se',scl'))
         (True, scl') -> 
           (V pre, let (V v, se') = e se in (v,se',scl'))) (v,ie,icl)
  
co_genpre
  :: CStream (Val t) s''
     -> CStream (Val t) s
     -> CStream Bool s'
     -> CStream (Val t) (t, s, s')
co_genpre (Co f iv) (Co e ie) (Co cl icl) =
  let (V v,_) = f iv in
  Co (\(pre,se,scl) -> case cl scl of 
         (False, scl') -> 
           let (E, se') = e se 
           in (E, (pre,se',scl'))
         (True, scl') -> 
           let (V v, se')   = e se 
           in (V pre, (v,se',scl'))) (v,ie,icl)

co_plus :: CStream Int s1 -> CStream Int s2 -> CStream Int (s1, s2)
co_plus (Co tx ix) (Co ty iy) = 
  Co (\(sx,sy) -> let (vx, sx') = tx sx
                      (vy, sy') = ty sy
                  in  (vx + vy, (sx', sy'))) (ix, iy)
  
co_op :: (a -> a -> a) -> CStream a s1 -> CStream a s2 -> CStream a (s1, s2)
co_op op (Co tx ix) (Co ty iy) = 
  Co (\(sx,sy) -> let (vx, sx') = tx sx
                      (vy, sy') = ty sy
                  in  (op vx vy, (sx', sy'))) (ix, iy)

co_unaryop :: (a -> a) -> CStream a s -> CStream a s
co_unaryop op (Co tx ix) = 
  Co (\sx -> let (vx, sx') = tx sx
             in  (op vx, sx')) ix

  
co_even :: CStream t s -> CStream t s
co_even (Co tx ix) =
  Co (\sx -> let (vx1, sx1) = tx sx
                 (vx2, sx2) = tx sx1 
             in  (vx2, sx2)) ix
  
  
data State s = Nil | St s

{-
co_const :: t -> CStream t (State s)
co_const v = Co (\s -> (v, Nil)) Nil
-}

co_const :: t -> CStream Bool s -> CStream (Val t) s
co_const v (Co tc ic) = 
  Co (\s -> case tc s of
         (True, s') -> (V v, s')
         (False, s') -> (E, s')) ic

{-
co_extend :: CStream (t1 -> t2) s2 -> CStream t1 s1 -> CStream t2 (s2, s1)
co_extend (Co f i) (Co e ie) = 
  Co (\(sf,se) -> let (vf, sf') = f sf
                      (ve, se') = e se 
                  in  (vf ve, (sf', se'))) (i,ie)

co_plus1 x y = co_extend (co_extend (co_const (+)) x) y

-}

co_extend :: CStream (Val (t1 -> t2)) s2
             -> CStream (Val t1) s1 -> CStream (Val t2) (s2, s1)
co_extend (Co f i) (Co e ie) = 
  Co (\(sf,se) -> 
       case (f sf, e se) of
         ((E, sf), (E, se)) -> (E, (sf,se))
         ((V vf, sf), (V ve, se)) -> (V (vf ve), (sf, se))) (i,ie)
  
{-
co_fby :: CStream t s2 -> CStream t s1 -> CStream t (State t, s2, s1)
(Co tx ix) `co_fby` (Co ty iy) = 
  Co (\(init,sx,sy) -> let (vx, sx') = tx sx
                           (vy, sy') = ty sy 
                       in  case init of
                         Nil  -> (vx, (St vy, sx', sy'))
                         St v -> (v, (St vy, sx', sy'))) (Nil, ix, iy)
-}

co_fby :: CStream (Val t) s2 -> CStream (Val t) s1 -> CStream (Val t) (State t, s2, s1)
(Co tx ix) `co_fby` (Co ty iy) =
  Co (\(init,sx,sy) -> case tx sx of
         (E, sx') -> (E, let (E,sy') = ty sy in (init,sx',sy'))
         (V vx, sx') -> (case init of Nil -> V vx; St v -> V v, 
                         let (V v, sy') = ty sy in (St v,sx',sy'))) (Nil,ix,iy)
  
  
co_when :: CStream (Val t) s -> CStream (Val Bool) sc -> CStream (Val t) (s, sc)
(Co tx ix) `co_when` (Co tc ic) =   
  Co (\(sx,sc) -> case (tx sx, tc sc) of
         ((E,sx'), (E,sc')) -> (E, (sx',sc'))
         ((V vx, sx'), (V True, sc')) -> (V vx, (sx',sc'))
         ((V vx, sx'), (V False, sc')) -> (E, (sx',sc'))) (ix,ic)
         
on :: CStream Bool s1 -> CStream (Val Bool) s2 -> CStream Bool (s1,s2)
(Co tcl icl) `on` (Co tc ic) = 
  Co (\(scl,sc) -> case tcl scl of 
         (False, scl') -> let (E, sc') = tc sc in (False, (scl',sc'))
         (True, scl') -> let (V vc, sc') = tc sc in (vc, (scl',sc'))) (icl,ic)
  
co_apply :: CStream (t1 -> State s -> (t, State s)) s1
            -> CStream t1 s2 -> CStream t (State s, s1, s2)
co_apply (Co tf sf) (Co te se) =   
  Co (\(st, sf, se) ->
       let (vf, sf') = tf sf
           (ve, se') = te se
           (v,  st') = vf ve st 
       in  (v, (st', sf', se'))) (Nil, sf, se)
  
{-
co_lambda
  :: (CStream t (State s) -> CStream t' s')
     -> CStream (t -> State s' -> F t' (State s')) (State sf)
co_lambda f = co_const (\v s -> let Co t i = f (co_const v)  
                                    s1     = case s of Nil -> i
                                                       St s -> s
                                    (v', s') = t s1 
                                in  (v', St s'))
-}
              
co_nat :: CStream Int Int
co_nat = Co (\s -> (s, s+1)) 0              


co_rec :: CStream (t -> State s -> F t (State s)) s' -> CStream t (State s, s')
co_rec (Co t i) =
  Co (\(se,s) -> 
       let (v, s') = t s
           (ve, se') = v ve se 
       in  (ve, (se',s'))) (Nil,i)
  
co_product :: CStream t1 s1 -> CStream t2 s2 -> CStream (t1,t2) (s1,s2)
co_product (Co e1 i1) (Co e2 i2) =   
  Co (\(s1,s2) -> let (v1, s1') = e1 s1
                      (v2, s2') = e2 s2 
                  in  ((v1,v2), (s1',s2'))) (i1,i2)
  
co_fst :: CStream (t1,t2) (s1,s2) -> CStream t1 (s1,s2)
co_fst (Co e (i1,i2)) =
  Co (\(s1,s2) -> let ((v1,v2), (s1',s2')) = e (s1, s2)
                  in  (v1,(s1',s2'))) (i1,i2)

co_snd :: CStream (t1,t2) (s1,s2) -> CStream t2 (s1,s2)
co_snd (Co e (i1,i2)) =
  Co (\(s1,s2) -> let ((v1,v2), (s1',s2')) = e (s1, s2)
                  in  (v2,(s1',s2'))) (i1,i2)

data Val t = E | V t  

co_merge
  :: CStream (Val Bool) sc
     -> CStream (Val t) s
     -> CStream (Val t) s'
     -> CStream (Val t) (sc, s, s')
co_merge (Co tc ic) (Co tx ix) (Co ty iy) = 
  Co (\(sc,sx,sy) ->
       case (tc sc, tx sx, ty sy) of
         ((E,sc'), (E,sx'), (E,sy')) -> (E, (sc',sx',sy'))
         ((V True, sc'), (V vx, sx'), (E, sy')) -> (V vx, (sc',sx',sy'))
         ((V False, sc'), (E, sx'), (V vy, sy')) -> (V vy, (sc',sx',sy'))) (ic,ix,iy)
  
  
  