{-# INLINE (>>=) #-}
(>>=) :: ((oa + sa) ~ ob) 
      => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
(>>=) = (>>>=)

{-# INLINE return #-}
return :: a -> Reader o 0 m a
return = pureReader
