{-# LANGUAGE CPP, DataKinds, RankNTypes, ScopedTypeVariable, 
             TypeFamilies, TypeOperators, TypeApplications, FlexibleContexts #-}

import ...

newtype Reader (o :: Nat) (s :: Nat) m a = Reader 
    { runReader :: forall r . Addr -> (a -> m (Result r)) -> m (Result r) }

{-# INLINE pureReader #-}
pureReader :: a -> Reader o 0 m a
pureReader a = Reader $ \_ k -> k a

{-# INLINE bindReader #-}
bindReader :: ((oa + sa) ~ ob) 
           => (a -> Reader ob sb m b) -> Reader oa sa m a -> Reader oa (sa + sb) m b
bindReader g (Reader f) = Reader $ \addr k -> f addr $ \a -> runReader (g a) addr k  

{-# INLINE (>>>=) #-}
(>>>=) :: ((oa + sa) ~ ob) 
       => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
(>>>=) = flip bindReader

instance Functor (Reader (o :: Nat) (s :: Nat) m) where 
    {-# INLINABLE fmap #-}
    fmap f = bindReader (pureReader . f)
