import System.Environment
import Srcs.Grill
import Srcs.Leakser
import Srcs.Heuristic
import Data.List as Dl
import Data.HashMap as Hm
import Data.Heap.Binary as Bh

-- import Debug.Trace


{--
Child = (Int   -> Heuristic
         Grill -> Current Grill
         Int   -> cost) 
--}
type Child = (Int, Grill, Int)

main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    (grill, res, hf) <- leakser args
    printGrill grill
    putStrLn "--------------"
    printGrill res
    putStrLn "--------------"
    printGrill $ aStar grill res hf

aStar :: Grill -> Grill -> Heuristic -> Grill
aStar grill res hf =
    let
        closeList = Hm.singleton grill 0
        openList = Bh.singleton (0, grill, 0)
    in aStarBis closeList openList res hf
        where
            aStarBis :: Map Grill Int -> BinaryHeap Child -> Grill -> Heuristic -> Grill
            aStarBis _ Leaf _ _ = []
            aStarBis closeList openList res hf
                | grill == res = grill
                | otherwise = aStarBis (Hm.insert grill 0 closeList) (Bh.merge (calcHf hf res $ tailR closeList xs cost $ createChildren grill) xs) res hf
                where 
                    (heur, grill, cost) = Bh.head openList
                    xs = Bh.tail openList
                    tailR :: Map Grill Int -> BinaryHeap Child -> Int -> [Grill] -> BinaryHeap Child
                    tailR _ _ _ [] = Leaf
                    tailR closeList openList cost (x:xs)
                        | member x closeList = tailR closeList openList cost xs
                        | otherwise = Bh.insert (0, x, cost + 1) (tailR closeList openList cost xs)
                    calcHf :: Heuristic -> Grill -> BinaryHeap Child -> BinaryHeap Child
                    calcHf _ _ Leaf = Leaf
                    calcHf hf res openList = Bh.insert (cost + (hf grill res), grill, cost) (calcHf hf res xs)
                        where
                            (heur, grill, cost) = Bh.head openList
                            xs = Bh.tail openList
                    createChildren :: Grill -> [Grill]
                    createChildren grill = [moveRight grill] ++ [moveLeft grill] ++ [moveUp grill] ++ [moveDown grill]