-- A GADT for arrays with type-indexed representation
data Arr e where
    ArrInt :: !Int -> ByteArray# -> Arr Int
    ArrPair :: !Int -> Arr e1 -> Arr e2 -> Arr (e1, e2)
  
  (!:) :: Arr e -> Int -> e
  {-# SPECIALISE INLINE (!:) :: Arr Int -> Int -> Int #-}
  {-# SPECIALISE INLINE (!:) :: Arr (a, b) -> Int -> (a, b) #-}
  (ArrInt _ ba)     !: (I# i) = I# (indexIntArray# ba i)
  (ArrPair _ a1 a2) !: i      = (a1 !: i, a2 !: i)
  