elefantes :: Int -> IO () 
elefantes n = do
                    if n>3 then
                        elefantes (n-1) 
                    else return ()

                    if(n>=3) then
                        putStr("Se " ++ show (n-1) ++ " elefantes incomodam muita gente,\n" ++ show n ++ " incomodam muito mais!\n")
                    else return ()
