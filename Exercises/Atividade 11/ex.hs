import Stack

searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = search' (push x empty)
    where
        search' s
            | isEmpty s = []
            | goal (top s) = top s : search' (pop s)
            | otherwise =
                let x = top s in search' (foldr push (pop s) (succ x))

type Column = Int
type Row = Int
type SolNQ = [(Column,Row)]
type NodeNQ = (Column,Column,SolNQ)

valid :: SolNQ -> (Column , Row) -> Bool
valid psol (c,r) = and (map test psol)
    where test (c',r') = and [c'+r'/=c+r,c'-r'/=c-r,r'/=r]

succNq :: NodeNQ -> [NodeNQ]
succNq (c,n,psol) = [(c+1,n,psol++[(c,r)]) | r <- [1..n], valid psol (c,r)]

goalNq :: NodeNQ -> Bool
goalNq (c,n,psol) = c > n

firstNq :: Column -> SolNQ
firstNq n = s
    where ((_,_,s):_) = searchDfs succNq goalNq (1,n,[])
