unfoldC :: Monad m
                => (b -> Maybe (a, b))
                -> b
                -> ConduitT i a m ()
unfoldC f =
    go
  where
    go seed =
        case f seed of
            Just (a, seed') -> yield a >> go seed'
            Nothing -> return ()
{-# INLINE unfoldC #-}
