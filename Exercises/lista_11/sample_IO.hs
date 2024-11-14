module Main where
main = do   putChar 'A'
            putChar 'B'
            (putChar 'A' >> putChar 'B') :: IO ()
