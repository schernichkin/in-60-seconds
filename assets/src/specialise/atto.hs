hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = T.foldl' step 0 `fmap` takeWhile1 isHexDigit
  where
    isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')
    step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where w = ord c
{-# SPECIALISE hexadecimal :: Parser Int #-}
{-# SPECIALISE hexadecimal :: Parser Int8 #-}
{-# SPECIALISE hexadecimal :: Parser Int16 #-}
{-# SPECIALISE hexadecimal :: Parser Int32 #-}
{-# SPECIALISE hexadecimal :: Parser Int64 #-}
{-# SPECIALISE hexadecimal :: Parser Integer #-}
{-# SPECIALISE hexadecimal :: Parser Word #-}
{-# SPECIALISE hexadecimal :: Parser Word8 #-}
{-# SPECIALISE hexadecimal :: Parser Word16 #-}
{-# SPECIALISE hexadecimal :: Parser Word32 #-}
{-# SPECIALISE hexadecimal :: Parser Word64 #-}