elefantes :: Int -> IO () 
elefantes n = do
                    if n==3 then return () else elefantes (n-1) 
                    putStr("Se " ++ show (n-1) ++ " elefantes incomodam muita gente,\n" ++ show n ++ " incomodam muito mais!\n")
                    return ()

