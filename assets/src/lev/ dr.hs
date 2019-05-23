newtype Reader c m a = Reader { 
    runReader :: forall r . c -> (c -> a -> m (Result r)) -> m (Result r) }

{-# INLINE pureReader #-}
pureReader :: a -> Reader c m a
pureReader a = Reader $ \c k -> k c a

{-# INLINE bindReader #-}
bindReader :: (a -> Reader c m b) -> Reader c m a -> Reader c m b
bindReader g (Reader f) = Reader $ \c0 k -> f c0 $ \c1 a -> runReader (g a) c1 k

instance Functor ...
instance Applicative ...
instance Monad ...

{-# INLINE fixed #-}
fixed :: forall s c m a . ( KnownNat s, Consumable c, PrimMonad m ) 
      => Fixed.Reader 0 s m a -> Reader c m a
fixed (Fixed.Reader f) = Reader $ \c0 k -> do
    let size = fromIntegral (natVal $ sing @s)
    consume c0 size $ \c1 addr -> f addr $ \a -> k c1 a

class Consumable c where
    consume :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> m (Result a)
