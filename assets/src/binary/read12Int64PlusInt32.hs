read12Int64PlusInt32 :: Get Int64
read12Int64PlusInt32 = do
  a1 <- getInt64host
  a2 <- getInt64host
  a3 <- getInt64host
  a4 <- getInt64host
  a5 <- getInt64host
  a6 <- getInt64host
  a7 <- getInt64host
  a8 <- getInt64host
  a9 <- getInt64host
  a10 <- getInt64host
  a11 <- getInt64host
  a12 <- getInt64host
  a13 <- getInt32host
  return $ a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8
         + a9 + a10 + a11 + a12 + fromIntegral a13
