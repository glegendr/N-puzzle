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
import Debug.Trace
import Actions

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

av :: Double -> (Double, Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double, Double)
av div (a, b, c, d, e, f) = (a / div, b / div, c / div, d / div, e / div, f / div)

tuppleToList :: (a, a, a, a, a, a) -> [a]
tuppleToList (a, b, c, d, e, f) = [a, b, c, d, e, f]

extremes :: [(Double, Double, Double, Double)] -> [(Double, Int)] -> ((Double, Double, Double, Double), (Double, Double, Double, Double))
extremes base lst = (base !! minLst, base !! maxLst)
    where
        (_, maxLst) = maximum lst
        (_, minLst) = minimum lst

minmax :: (Double, Double, Double, Double, Double, Double) -> [(String, (Double, Double, Double, Double, Double, Double))] -> [(String, (String, String, String, String, String, String))]
minmax _ [] = []
minmax allMax@(maxA, maxB, maxC, maxD, maxE, maxF) ((str, (a, b, c, d, e, f)):xs) = (tuppleToString (str, (10 * (log a) / (log maxA), 10 * (log b) / (log maxB), 10 * (c^2) / (maxC^2), 10 * ( d) / ( maxD), 10 * (log $ 1 + e) / (log $ 1 + maxE), 10 * (log $ 1 + f) / (log $ 1 + maxF)))) : minmax allMax xs

tuppleToString :: (String, (Double, Double, Double, Double, Double, Double)) -> (String, (String, String, String, String, String, String))
tuppleToString (str, (a, b, c, d, e, f)) = (str, (show a, show b, show c, show d, show e, show f))

toJson3 :: [(String, String)] -> String
toJson3 [] = []
toJson3 ((name, value):[]) = "\t\t\"" ++ name ++ "\": \"" ++ value ++  "\"\n"
toJson3 ((name, value):xs) = "\t\t\"" ++ name ++ "\": \"" ++ value ++  "\",\n" ++ toJson3 xs

toJson2 :: [[(String, String)]] -> String
toJson2 [] = "]"
toJson2 (x:[]) = "\t{\n" ++ toJson3 x ++ "\t}\n" ++ toJson2 []
toJson2 (x:xs) = "\t{\n" ++ toJson3 x ++ "\t},\n" ++ toJson2 xs

toJson :: String -> [(String, (Double, Double, Double, Double, Double, Double))] -> IO ()
toJson path lst = writeFile path $ "[\n" ++ (toJson2 $ foldl (zipAll fieldsName) [] $ minmax allMax lst)
    where -- transform all Double to String
        newLst = map (\(_, tupple) -> tupple) lst
        allMax = foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (max acA a, max acB b, max acC c, max acD d, max acE e, max acF f)) newLst
        fieldsName = ["name", "timeComplexity", "memoryComplexity", "strokes", "branchDepth", "time", "complexity"]
        zipAll :: [String] -> [[(String, String)]] -> (String, (String, String, String, String, String, String)) -> [[(String, String)]]
        zipAll strs acc (name, tupple) = (zip strs $ name : (tuppleToList tupple)) : acc

benchmark :: IO ()
benchmark = do
    (_, res2) <- parse "MapSolved/Map3x3"
    let allMaps = generateMap 3 2 res2
    let zipped = zip (repeat (aStar2 res2 (algorithmFunction (+) manhattan res2))) allMaps
    let zipped2 = zip (repeat (aStar2 res2 (algorithmFunction (+) (wManhattan 3) res2))) allMaps
    let zipped3 = zip (repeat (aStar2 res2 (algorithmFunction (+) (wManhattan 8) res2))) allMaps
    let zipped4 = zip (repeat (aStar2 res2 (algorithmFunction (+) euclidean res2))) allMaps
    let zipped5 = zip (repeat (aStar2 res2 (algorithmFunction (+) dijkstra res2))) allMaps
    lst  <- foldl benchOnMap initBench zipped
    lst2 <- foldl benchOnMap initBench zipped2
    lst3 <- foldl benchOnMap initBench zipped3
    lst4 <- foldl benchOnMap initBench zipped4
    lst5 <- foldl benchOnMap initBench zipped5
    let av1 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst
    -- [timeComplexity, MemoryComplexity, Strokes, BranchDepth, Time], acF
    let av2 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst2
    let av3 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst3
    let av4 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst4
    let av5 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst5
    toJson "chart/data.json" [("Manhattan", av1), ("Manhattan3", av2), ("Manhattan8", av3), ("Euclidean", av4), ("Dijkstra", av5)]
    -- let (extMin, extMax) = extremes lst $ zip (map (\(a, b, c, d) -> a + b + c + d) $ map (minmax $ generateMax lst) lst) [0..]
    -- let (extMin2, extMax2) = extremes lst2 $ zip (map (\(a, b, c, d) -> a + b + c + d) $ map (minmax $ generateMax lst2) lst2) [0..]
    -- let (extMin3, extMax3) = extremes lst3 $ zip (map (\(a, b, c, d) -> a + b + c + d) $ map (minmax $ generateMax lst3) lst3) [0..]
    -- let (extMin4, extMax4) = extremes lst4 $ zip (map (\(a, b, c, d) -> a + b + c + d) $ map (minmax $ generateMax lst4) lst4) [0..]
    -- spiderChartEasy "4Manhattan3x3" [("Min", extMin), ("Average", average), ("Max", extMax)]
    -- spiderChartEasy "8Manhattan3x3" [("Min", extMin2), ("Average", average2), ("Max", extMax2)]
    -- spiderChartEasy "16Manhattan3x3" [("Min", extMin3), ("Average", average3), ("Max", extMax3)]
    -- spiderChartEasy "32Manhattan3x3" [("Min", extMin4), ("Average", average4), ("Max", extMax4)]


    -- print average2

    -- args <- getArgs
    -- (_, grill) <- parse "test-files/valids/map-txt"
    -- (_, grill2) <- parse "test-files/valids/01-map"
    -- (_, res) <- parse "MapSolved/Map4x4"
    -- (_, res2) <- parse "MapSolved/Map3x3"
    -- checkGrill grill res
    -- lst1 <- foldl calcBench initBench (createData algo (init heuristic) grill res "map-txt")
    -- lst2 <- foldl calcBench initBench (createData algo heuristic grill2 res2 "01-map")
    -- let lst = lst1 ++ lst2
    --     chart = getVFlag args
    -- putStrLn "Time Complexity:"
    -- printBench $ map (\(name, x, _, _, _) -> (name, x)) $ sortOn (\(_, x, _, _, _) -> x) lst
    -- putStrLn "\nMemory Complexity:"
    -- printBench $ map (\(name, _, x, _, _) -> (name, x)) $ sortOn (\(_, _, x, _, _) -> x) lst
    -- putStrLn "\nNumber of moves:"
    -- printBench $ map (\(name, _, _, x, _) -> (name, x)) $ sortOn (\(_, _, _, x, _) -> x) lst
    -- putStrLn "\nTime:"
    -- printBench $ map (\(name, _, _, _, x) -> (name, x)) $ sortOn (\(_, _, _, _, x) -> x) lst
    -- if chart == True
    -- then chartMe lst1 lst2
    -- else return ()
    exitWith ExitSuccess

aStar2 res hf grill = aStarBench grill res hf 

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

benchOnMap :: IO [(Double, Double, Double, Double, Double, Double)] -> ((Grill -> ([Act], [[Act]], Int, Int)), Grill) -> IO [(Double, Double, Double, Double, Double, Double)]
benchOnMap acc (fct, grill) = do
        time <- getCurrentTime
        let (moves, acts, timeC, mem) = fct grill
        time2 <- timeC `deepseq` getCurrentTime
        let strokes = realToFrac $ length moves
        let retTime = (read (filter (\x -> x `elem` ['0'..'9'] || x == '.') $ show $ diffUTCTime time2 time)) :: Double
        y <- acc
        return ((realToFrac timeC, realToFrac mem, strokes, (foldl (\acc x -> acc + (realToFrac $ length x)) 0 acts) / (realToFrac $ length acts), retTime, strokes * retTime) : y)

-- initBench :: IO [((String, String, String), Double, Double, Double, Double)]
initBench = return []

generateMap :: Int -> Int -> Grill -> [Grill]
generateMap size number res =  take number $  filter (checkGrillBool res) $ map toGrill $ permutations [0..(size^2 - 1)]