{-# LANGUAGE ... RankNTypes ... #-}

newtype Get a = C { runCont :: forall r.
                               B.ByteString ->
                               Success a r ->
                               Decoder   r }

type Success a r = B.ByteString -> a -> Decoder r

data Decoder a = Fail !B.ByteString String
              | Partial (Maybe B.ByteString -> Decoder a)
              | Done !B.ByteString a
              | BytesRead {-# UNPACK #-} !Int64 (Int64 -> Decoder a)

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \i ks -> c i (\i' a -> (runCont (f a)) i' ks)

runGet :: Get a -> L.ByteString -> a
runGet g lbs0 = feedAll (runGetIncremental g) lbs0
  where
  feedAll (Done _ _ x) _ = x
  feedAll (Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
  feedAll (Fail _ pos msg) _ =
    error ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)
