newtype Peek a = Peek
    { runPeek :: PeekState -> Ptr Word8 -> IO (PeekResult a)
    } deriving (Functor)

newtype PeekState = PeekState
    { peekStateEndPtr :: Ptr Word8 }

data PeekResult a = PeekResult {-# UNPACK #-} !(Ptr Word8) !a
    deriving (Functor)

Peek x >>= f = Peek $ \end ptr1 -> do
        PeekResult ptr2 x' <- x end ptr1
        runPeek (f x') end ptr2
{-# INLINE (>>=) #-}
