module Benchmark
(benchmark) where

import Heuristic
import Data.Time
import Algo
import Grill
import Parsing
import Data.List
import Debug.Trace
import System.Exit
import Text.Printf
import Control.DeepSeq

{--
    [(String, Heuristic)]
    [(String, Algo)]

    [(String, String, String Int, Int, Int, NominalDiffTime)]
    String          -> Heuristic's Name
    String          -> Algo's Name
    String          -> Map's Name
    Int             -> Time Complexity
    Int             -> Memory Complexity
    Int             -> Number of moves
    NominalDiffTime -> time
--}
algo = [("aStar", (+)), ("wAStar", (\x y -> x + 8 * y)), ("multStar", (*))]
heuristic = [("manhattan", manhattan), ("wManhattan", wManhattan 4), ("euclidean", euclidean), ("wEuclidean", wEuclidean 4), ("dijkstra", dijkstra)]

createData :: [a] -> [b] -> Grill -> Grill -> String -> [(a, b, Grill, Grill, String)]
createData [] _ _ _ _ = []
createData (x:xs) lst grill res str = (mergeData x lst grill res str) ++ (createData xs lst grill res str) 
    where
        mergeData :: a -> [b] -> Grill -> Grill -> String -> [(a, b, Grill, Grill, String)]
        mergeData x [] _ _ _ = []
        mergeData x (y:ys) grill res str = (x, y, grill, res, str) : mergeData x ys grill res str

printBench :: Show(a) => [((String, String, String), a)] -> IO ()
printBench [] = return ()
printBench (((n1, n2, n3), x):xs) = do
    printf "%-10s%-15s%-8s -> " n1 n2 n3
    print x
    printBench xs

benchmark :: IO ()
benchmark = do
    (_, grill) <- parse "test-files/valids/map-txt"
    (_, grill2) <- parse "test-files/valids/01-map"
    (_, res) <- parse "MapSolved/Map4x4"
    (_, res2) <- parse "MapSolved/Map3x3"
    checkGrill grill res
    lst1 <- foldl calcBench initBench (createData algo (init heuristic) grill res "map-txt")
    lst2 <- foldl calcBench initBench (createData algo heuristic grill2 res2 "01-map")
    let lst = lst1 ++ lst2
    putStrLn "Time Complexity:"
    printBench $ map (\(name, x, _, _, _) -> (name, x)) $ sortOn (\(_, x, _, _, _) -> x) lst
    putStrLn "\nMemory Complexity:"
    printBench $ map (\(name, _, x, _, _) -> (name, x)) $ sortOn (\(_, _, x, _, _) -> x) lst
    putStrLn "\nNumber of moves:"
    printBench $ map (\(name, _, _, x, _) -> (name, x)) $ sortOn (\(_, _, _, x, _) -> x) lst
    putStrLn "\nTime:"
    printBench $ map (\(name, _, _, _, x) -> (name, x)) $ sortOn (\(_, _, _, _, x) -> x) lst
    exitWith ExitSuccess

calcBench :: IO [((String, String, String), Int, Int, Int, NominalDiffTime)] -> ((String, (Int -> Int -> Int)), (String, Heuristic), Grill, Grill, String) -> IO [((String, String, String), Int, Int, Int, NominalDiffTime)]
calcBench acc ((nameA, algoF), (nameB, heuristicF), grill, res, mapName) = do
    time <- getCurrentTime
    let (moves, timeC, mem) =  aStar grill res (algorithmFunction algoF heuristicF res)
    time2 <- timeC `deepseq` getCurrentTime
    y <- acc
    return (((nameA, nameB, mapName), timeC, mem, length moves, diffUTCTime time2 time) : y)

initBench :: IO [((String, String, String), Int, Int, Int, NominalDiffTime)]
initBench = return []
