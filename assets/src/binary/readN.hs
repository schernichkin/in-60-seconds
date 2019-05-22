readN :: Int -> (B.ByteString -> a) -> Get a
readN !n f = ensureN n >> unsafeReadN n f
{-# INLINE [0] readN #-}

{-# RULES

"readN/readN merge" forall n m f g.
  apG (readN n f) (readN m g) = readN (n+m) (\bs -> f bs $ g (B.unsafeDrop n bs)) #-}

  ensureN :: Int -> Get ()
  ensureN !n0 = C $ \inp ks -> do
    if B.length inp >= n0
      then ks inp ()
      else runCont (withInputChunks n0 enoughChunks onSucc onFail >>= put) inp ks
    where 
      enoughChunks n str
        | B.length str >= n = Right (str,B.empty)
        | otherwise = Left (n - B.length str)
      onSucc = B.concat . dropWhile B.null
      onFail bss = C $ \_ _ -> Fail (B.concat bss) "not enough bytes"
{-# INLINE ensureN #-}

unsafeReadN :: Int -> (B.ByteString -> a) -> Get a
unsafeReadN !n f = C $ \inp ks -> do
  ks (B.unsafeDrop n inp) $! f inp 