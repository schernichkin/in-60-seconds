{-# INLINE prim #-}
prim :: forall o s m a . (KnownNat o, PrimMonad m, Prim a) 
     => Reader o s m a 
prim = Reader $ \addr k -> readOffAddr (addr `plusAddr` off) 0 >>= k
    where
        off = fromIntegral $ natVal $ sing @o

#include "MachDeps.h"

#define PRIM(F,A,S) \
{-# INLINABLE F #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S IO A #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S (ST s) A #-}; \
F :: forall o m . (KnownNat o, PrimMonad m) => Reader o S m A; \
F = prim

PRIM(readWord8, Word8, SIZEOF_WORD8)
PRIM(readWord16, Word16, SIZEOF_WORD16)
PRIM(readWord32, Word32, SIZEOF_WORD32)
PRIM(readWord64, Word64, SIZEOF_WORD64)
