module Benchmark
(benchmark) where

import System.Environment
import Heuristic
import Data.Time
import Algo
import Grill
import Parsing
import Data.List
import System.Exit
import Text.Printf
import Control.DeepSeq
import Chart

{--
    [(String, Heuristic)]
    [(String, Algo)]

    [(String, String, String Double, Double, Double, NominalDiffTime)]
    String          -> Algo's Name
    String          -> Heuristic's Name
    String          -> Map's Name
    Double          -> Time Complexity
    Double          -> Memory Complexity
    Double          -> Number of moves
    Double          -> time
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

getVFlag :: [String] -> Bool
getVFlag [] = False
getVFlag (x:xs) 
    | x == "-v" || x == "--visual" = True
    | otherwise = getVFlag xs

benchmark :: IO ()
benchmark = do
    args <- getArgs
    (_, grill) <- parse "test-files/valids/map-txt"
    (_, grill2) <- parse "test-files/valids/01-map"
    (_, res) <- parse "MapSolved/Map4x4"
    (_, res2) <- parse "MapSolved/Map3x3"
    checkGrill grill res
    lst1 <- foldl calcBench initBench (createData algo (init heuristic) grill res "map-txt")
    lst2 <- foldl calcBench initBench (createData algo heuristic grill2 res2 "01-map")
    let lst = lst1 ++ lst2
        chart = getVFlag args
    putStrLn "Time Complexity:"
    printBench $ map (\(name, x, _, _, _) -> (name, x)) $ sortOn (\(_, x, _, _, _) -> x) lst
    putStrLn "\nMemory Complexity:"
    printBench $ map (\(name, _, x, _, _) -> (name, x)) $ sortOn (\(_, _, x, _, _) -> x) lst
    putStrLn "\nNumber of moves:"
    printBench $ map (\(name, _, _, x, _) -> (name, x)) $ sortOn (\(_, _, _, x, _) -> x) lst
    putStrLn "\nTime:"
    printBench $ map (\(name, _, _, _, x) -> (name, x)) $ sortOn (\(_, _, _, _, x) -> x) lst
    if chart == True
    then chartMe lst1 lst2
    else return ()
    exitWith ExitSuccess

calcBench :: IO [((String, String, String), Double, Double, Double, Double)] -> ((String, (Int -> Int -> Int)), (String, Heuristic), Grill, Grill, String) -> IO [((String, String, String), Double, Double, Double, Double)]
calcBench acc ((nameA, algoF), (nameB, heuristicF), grill, res, mapName)
    | nameA == "aStar" && nameB == "euclidean" = do
        y <- acc
        return y 
    | otherwise = do
        time <- getCurrentTime
        let (moves, timeC, mem) =  aStar grill res (algorithmFunction algoF heuristicF res)
        time2 <- timeC `deepseq` getCurrentTime
        y <- acc
        return (((nameA, nameB, mapName), realToFrac timeC, realToFrac mem, realToFrac $ length moves, read $ filter (\x -> x `elem` ['0'..'9'] || x == '.') $ show $ diffUTCTime time2 time) : y)

initBench :: IO [((String, String, String), Double, Double, Double, Double)]
initBench = return []
