read12Int64PlusInt32 :: ByteString -> (Int64, ByteString)
read12Int64PlusInt32 buffer = 
    case toForeignPtr buffer of (fbase, I# off, I# len) ->
            unsafeInlineIO $ withForeignPtr fbase $ \(Ptr baseAddr) -> do
            let !addr = plusAddr# baseAddr off
                !a1  = indexInt64OffAddr# addr 0#
                !a2  = indexInt64OffAddr# (plusAddr# addr  8#) 0#
                !a3  = indexInt64OffAddr# (plusAddr# addr 16#) 0#
                ...
                !a13 = indexInt32OffAddr# (plusAddr# addr 96#) 0#
            return ( I64# ( a1 +# a2 +# a3 +# a4 +# a5 +# a6 +# a7 +# 
                            a8 +# a9 +# a10 +# a11 +# a12 +# a13), 
                    fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )
