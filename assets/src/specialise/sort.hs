{-# SPECIALISE sort :: [Int] -> [Int] #-}
sort :: (Ord a) => [a] -> [a]
sort = ...