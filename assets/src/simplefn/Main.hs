data Result = Done {-# UNPACK #-} !Int | Fail 

{-# NOINLINE ask #-}
ask :: Int -> Result
ask a = if (a /= 42) then Done a else Fail

main :: IO ()
main = case (ask 43) of
    Done _ -> putStrLn "done"
    Fail -> putStrLn "error!"
