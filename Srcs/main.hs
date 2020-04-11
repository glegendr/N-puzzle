import System.Environment
import Srcs.Grill
import Srcs.Leakser
import Srcs.Heuristic
import Data.List as Dl
import Data.HashMap as Hm
import Data.Heap.Binary as Bh
import Text.Printf

-- import Debug.Trace

data Act = ActLeft | ActUp | ActRight | ActDown deriving (Eq, Ord)
instance Show Act where
    show ActLeft = "Left"
    show ActUp = "Up"
    show ActRight = "Right"
    show ActDown = "Down"
{--
Child = (Int   -> Heuristic
         Grill -> Current Grill
         Int   -> cost) 
--}
type Child = (Int, Grill, Int, [Act])
type Tmp = ((Int -> Int -> Int) -> Heuristic -> Grill -> Int -> Grill -> Int)
type Tmp2 = Int -> Grill -> Int

main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    (grill, res, hf, af) <- leakser args
    printGrill grill
    putStrLn "--------------"
    printGrill res
    putStrLn "--------------"
    let (moves, time, mem) = aStar grill res (algorithmFunction af hf res)
    putStrLn $ "Time Complexity: " ++ (show time)
    putStrLn $ "Memory Complexity: " ++ (show mem)
    putStrLn $ "Number of moves: " ++ (show (Dl.length moves))
    putStrLn $ "Moves: " ++ (show $ reverse moves)

aStar :: Grill -> Grill -> Tmp2 -> ([Act], Int, Int)
aStar grill res hf =
    let
        closeList = Hm.singleton grill 0
        openList = Bh.singleton (0, grill, 0, [])
    in aStarBis closeList openList res hf 0 0

aStarBis :: Map Grill Int -> BinaryHeap Child -> Grill -> Tmp2 -> Int -> Int -> ([Act], Int, Int)
aStarBis _ Leaf _ _ time mem = ([], time, mem)
aStarBis closeList openList res tmp time mem
    | grill == res = (act, time + Bh.length openList, mem)
    | otherwise = 
        let children = createChildren grill
            newMem = mem + (checkChildren children closeList) - 1
        in  aStarBis (Hm.insert grill 0 closeList) (Bh.merge (calcHf tmp $ tailR closeList xs cost act children) xs) res tmp (time + 1) newMem
    where
        (heur, grill, cost, act) = Bh.head openList
        xs = Bh.tail openList

checkChildren :: [(Grill, Act)] -> Map Grill Int -> Int
checkChildren [] _ = 0
checkChildren ((x, act):xs) closeList
    | member x closeList = 0 + checkChildren xs closeList
    | otherwise = 1 + checkChildren xs closeList 

tailR :: Map Grill Int -> BinaryHeap Child -> Int -> [Act] -> [(Grill, Act)] -> BinaryHeap Child
tailR _ _ _ _ [] = Leaf
tailR closeList openList cost oldAct ((x, act):xs)
    | member x closeList = tailR closeList openList cost oldAct xs
    | otherwise = Bh.insert (0, x, cost + 1, act : oldAct) (tailR closeList openList cost oldAct xs)

calcHf :: Tmp2 -> BinaryHeap Child -> BinaryHeap Child
calcHf _ Leaf = Leaf
calcHf tmp openList = Bh.insert (tmp cost grill, grill, cost, act) (calcHf tmp xs)
    where
        (heur, grill, cost, act) = Bh.head openList
        xs = Bh.tail openList

createChildren :: Grill -> [(Grill, Act)]
createChildren grill = [(moveRight grill, ActRight)] ++ [(moveLeft grill, ActLeft)] ++ [(moveUp grill, ActUp)] ++ [(moveDown grill, ActDown)]

algorithmFunction :: Tmp
algorithmFunction operator hf res cost grill = operator cost (hf grill res)