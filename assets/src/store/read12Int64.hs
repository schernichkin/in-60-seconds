{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Int -> Peek Int64
read12Int64PlusInt32 = loop 0
    where 
        loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
            a1 <- peek :: Peek Int64
            a2 <- peek :: Peek Int64
            a3 <- peek :: Peek Int64
            ...
            a13 <- peek :: Peek Int32
            loop (s + a1 + a2 + a3 + a4
                    + a5 + a6 + a7 + a8
                    + a9 + a10 + a11 + a12
                    + fromIntegral a13) (n - 1)
