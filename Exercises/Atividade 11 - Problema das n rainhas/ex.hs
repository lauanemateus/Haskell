import Stack

searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = search' (push x empty) -- succ e goal são funções de ordem superior
    where
        search' s
            | isEmpty s = []
            | goal (top s) = top s : search' (pop s)
            | otherwise = 
                let x = top s in search' (foldr push (pop s) (succ x))

type Column = Int
type Row = Int
type SolNq = [Row]
type NodeNQ = (Column,Column,SolNq) -- representa solução parcial: (1) coluna para a próxima posição, (2) o tamanho do tabuleiro, e (3) uma solução parcial

valid :: SolNq -> (Column , Row) -> Bool
valid psol (c,r) = and (map test (zip [1.. (length psol)] psol))
    where test (c',r') = and [c'+r'/=c+r,c'-r'/=c-r,r'/=r]

succNq :: NodeNQ -> [NodeNQ]
succNq (c,n,psol) = [(c+1,n,psol++[r]) | r <- [1..n], valid psol (c,r)]

goalNq :: NodeNQ -> Bool
goalNq (c,n,psol) = c > n

firstNq :: Column -> SolNq
firstNq n = s
    where ((_,_,s):_) = searchDfs succNq goalNq (1,n,[])

printBoard :: SolNq -> IO()
printBoard sol = do
                    putStr("------------------------------------\n")
                    putStr ("Posicao valida para " ++ show n ++ " rainhas no tabuleiro\n")
                    printBoardAcc [ posicoes (c, r) | r<-[1..n], c<-[1..n]]
                    putStr("------------------------------------\n")
    where   
        printBoardAcc :: [(Char, Column, Row)] -> IO ()
        printBoardAcc [] = return ()
        printBoardAcc ((x, c, r):xs) 
            | c == n = do
                            (putChar x >> putChar '\n')
                            printBoardAcc xs
            | otherwise = do
                            putChar x
                            printBoardAcc xs  
        n = length sol
        posicoes (c, r) 
            | elem (c, r) (zip [1.. (length sol)] sol) = ('x', c, r)
            | otherwise = ('.', c, r)  

solNq :: Int -> [SolNq]
solNq n = solNqAcc n 
    where 
        solNqAcc n = [s | (_,_,s) <- (searchDfs succNq goalNq (1,n,[]))]
