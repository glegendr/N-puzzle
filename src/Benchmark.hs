module Benchmark
(benchmark, getVFlag) where

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
import Data.List.Split

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
algo = [("aStar", (+)), ("wAStar", (\x y -> x + 8 * y)), ("multStar", (*)), ("averageStar", (\x y -> ((x + y) `div` 2) ^ 2))]
heuristic = [("manhattan", manhattan), ("wManhattan", wManhattan 4), ("euclidean", euclidean), ("wEuclidean", wEuclidean 4), ("dijkstra", dijkstra)]


getVFlag :: [String] -> String
getVFlag [] = []
getVFlag (x:x1:xs) 
    | x == "-v" || x == "--visual" = x1
    | head eq == "-v" || head eq == "--visual" = last eq
    | otherwise = getVFlag (x1:xs)
    where eq = splitOn "=" x
getVFlag (x:xs)
    | x == "-v" || x == "--visual" = error $ "No number provided after " ++ x ++ " flag"
    | head eq == "-v" || head eq == "--visual" = last eq
    | otherwise = getVFlag xs
    where eq = splitOn "=" x

helper :: IO ()
helper = do
    name <- getProgName
    putStrLn $ "Example: ./" ++ name ++ " -b -v=<value> -r\n"
    putStrLn $ "-v --visual <value>\t\t   Allow you to change site datas:\n\t\t\t\t   Example: ./" ++ name ++ " -b -v=\"dijkstra+euclidean4+manhattan\"\n\t\t\t\t   A new algorithm will generate a new page in the site with all datas"
    putStrLn $ "-r --regenerate\t\t\t   Regenerate all site data"
    exitWith ExitSuccess

checkHFlag :: [String] -> IO ()
checkHFlag [] = return ()
checkHFlag (x:xs)
    | x == "-h" || x == "--help" = helper
    | otherwise = checkHFlag xs

checkRFlag :: [String] -> IO ()
checkRFlag [] = return ()
checkRFlag (x:xs)
    | x == "-r" || x == "--regenerate" = regenerate
    | otherwise = checkRFlag xs

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
toJson3 ((name, value):[])
    | "Scope" `isSuffixOf` name = "\t\t\"" ++ (take (length name - 5) name) ++ "\": " ++ value ++  "\n"
    | otherwise = "\t\t\"" ++ name ++ "\": \"" ++ value ++  "\"\n"
toJson3 ((name, value):xs)
    | "Scope" `isSuffixOf` name = "\t\t\"" ++ (take (length name - 5) name) ++ "\": " ++ value ++  ",\n" ++ toJson3 xs
    | otherwise = "\t\t\"" ++ name ++ "\": \"" ++ value ++  "\",\n" ++ toJson3 xs

toJson2 :: [[(String, String)]] -> String
toJson2 [] = []
toJson2 (x:[]) = "\t{\n" ++ toJson3 x ++ "\t}\n" ++ toJson2 []
toJson2 (x:xs) = "\t{\n" ++ toJson3 x ++ "\t},\n" ++ toJson2 xs

toJson :: String -> [String] -> [(String, (Double, Double, Double, Double, Double, Double))] -> IO ()
toJson path fieldsName lst = writeFile path $ "[\n" ++ (toJson2 $ foldl (zipAll fieldsName) [] $ minmax allMax lst) ++ "]"
    where -- transform all Double to String
        newLst = map (\(_, tupple) -> tupple) lst
        allMax = foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (max acA a, max acB b, max acC c, max acD d, max acE e, max acF f)) newLst
        zipAll :: [String] -> [[(String, String)]] -> (String, (String, String, String, String, String, String)) -> [[(String, String)]]
        zipAll strs acc (name, tupple) = (zip strs $ name : (tuppleToList tupple)) : acc

toJsonV2 :: [(String, [[(String, String)]])] -> String
toJsonV2 [] = []
toJsonV2 ((name, lst):[]) = "{\n\"name\": \"" ++ name ++ "\",\n\"values\": [\n" ++ toJson2 lst ++ "]\n}\n"
toJsonV2 ((name, lst):xs) = "{\n\"name\": \"" ++ name ++ "\",\n\"values\": [\n" ++ toJson2 lst ++ "]\n},\n" ++ toJsonV2 xs 

makeAverage :: Heuristic -> IO ()
makeAverage hf = do
    (_, res2) <- parse "MapSolved/Map3x3"
    let allMaps = generateMap 3 10 res2
    let zipped = zip (repeat (aStar2 res2 (algorithmFunction (+) manhattan res2))) allMaps
    let zipped2 = zip (repeat (aStar2 res2 (algorithmFunction (+) (wManhattan 3) res2))) allMaps
    let zipped3 = zip (repeat (aStar2 res2 (algorithmFunction (+) (wManhattan 8) res2))) allMaps
    let zipped4 = zip (repeat (aStar2 res2 (algorithmFunction (+) euclidean res2))) allMaps
    let zipped5 = zip (repeat (aStar2 res2 (algorithmFunction (+) dijkstra res2))) allMaps
    let zipped6 = zip (repeat (aStar2 res2 (algorithmFunction (+) hf res2))) allMaps
    lst  <- foldl benchOnMap initBench zipped
    lst2 <- foldl benchOnMap initBench zipped2
    lst3 <- foldl benchOnMap initBench zipped3
    lst4 <- foldl benchOnMap initBench zipped4
    lst5 <- foldl benchOnMap initBench zipped5
    lst6 <- foldl benchOnMap initBench zipped6
    let av1 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst
    -- [timeComplexity, MemoryComplexity, Strokes, BranchDepth, Time], acF
    let av2 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst2
    let av3 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst3
    let av4 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst4
    let av5 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst5
    let av6 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst6
    toJson "docs/data.json" ["name", "timeComplexity", "memoryComplexity", "strokes", "branchDepth", "time", "complexity"] [("Manhattan", av1), ("Manhattan3", av2), ("Manhattan8", av3), ("Euclidean", av4), ("Dijkstra", av5), ("TemporaryValue", av6)]


oui :: [String] -> [(Double, Double, Double, Double, Double, Double)] -> [(String, [[(String, String)]])]
oui names lst = oui4 $ oui3 $ oui2 $ map (\(name, lst) -> (name, map (\(name, x) -> (name, show x)) {--$ reverse $ sortOn (\(_, x) -> x)--} $ zip algoName lst))$ zip names [tC, mC, st, bD, t, c]
    where
        oui2 = map (\(name, lst) -> (name, map (\(name2, x) -> [("value", x), ("label", name2)]) lst))
        oui3 = map (\(name, lst) -> (name, zipWith (\index lst2 -> ("index", show $ index) : lst2) (map (/10) [4 .. ]) lst))
        oui4 = map (\(name, lst) -> (name, zipWith (\index lst2 -> ("fill", index) : lst2) colors lst))
        algoName = ["AStar", "AStar3", "AStar8", "MultStar", "AverageStar"]
        colors = ["#231F20", "#403437", "#53363C", "#5E2C3A", "#660E34", "#7D3A4D", "#96606B", "#B28A91", "#D3BCBF", "#EDE4E5"]
        tC = map (\(x, _, _, _, _, _) -> x) lst
        mC = map (\(_, x, _, _, _, _) -> x) lst
        st = map (\(_, _, x, _, _, _) -> x) lst
        bD = map (\(_, _, _, x, _, _) -> x) lst
        t = map (\(_, _, _, _, x, _) -> x) lst
        c = map (\(_, _, _, _, _, x) -> x) lst

makeBarChart :: Heuristic -> String -> Int -> IO ()
makeBarChart hf name size = do
    (_, res2) <- parse "MapSolved/Map3x3"
    let allMaps = generateMap 3 size res2
    let zipped = trace (name ++ ": Generating AStar 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (+) hf res2))) allMaps
    let zipped2 = trace (name ++ ": Generating AStar3 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (\x y -> x + 3 * y) hf res2))) allMaps
    let zipped3 = trace (name ++ ": Generating AStar8 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (\x y -> x + 8 * y) hf res2))) allMaps
    let zipped4 = trace (name ++ ":  Generating Multstar 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (*) hf res2))) allMaps
    let zipped5 = trace (name ++ ":  Generating AverageStar 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (\x y -> ((x + y) `div` 2) ^ 2) hf res2))) allMaps
    lst  <- foldl benchOnMap initBench zipped
    lst2 <- foldl benchOnMap initBench zipped2
    lst3 <- foldl benchOnMap initBench zipped3
    lst4 <- foldl benchOnMap initBench zipped4
    lst5 <- foldl benchOnMap initBench zipped5
    -- [timeComplexity, MemoryComplexity, Strokes, BranchDepth, Time, complexity], acF
    let av1 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst
    let av2 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst2
    let av3 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst3
    let av4 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst4
    let av5 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst5
    let ret = oui ["TimeComplexity", "MemoryComplexity", "Strokes", "BranchDepth", "Time", "Complexity"] [av1, av2, av3, av4, av5]
    writeFile name $ "[\n" ++ toJsonV2 ret ++ "]"
    -- toJson "docs/data.json" ["name", "timeComplexity", "memoryComplexity", "strokes", "branchDepth", "time", "complexity"] [("AStar", av1), ("Manhattan3", av2), ("Manhattan8", av3), ("Euclidean", av4)]

oui2 :: [(String, [(Double, Double)])] -> [[(String, String)]]
oui2 [] = []
oui2 ((name, lst):xs) = [("name", name), ("timeScope", show timeList), ("complexityScope", show compList)] : oui2 xs
    where
        timeList = map (\(time, _) -> time) lst
        compList = map (\(_, comp) -> comp) lst

makeLineChart :: Heuristic -> String -> Int -> IO ()
makeLineChart hf name size = do
    (_, res2) <- parse "MapSolved/Map3x3"
    let allMaps = generateMap 3 size res2
    let zipped = trace (name ++ ": Generating AStar 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (+) hf res2))) allMaps
    let zipped2 = trace (name ++ ": Generating AStar3 0/" ++ show size)  $ zip (repeat (aStar2 res2 (algorithmFunction (\x y -> x + 3 * y) hf res2))) allMaps
    let zipped3 = trace (name ++ ": Generating AStar8 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (\x y -> x + 8 * y) hf res2))) allMaps
    let zipped4 = trace (name ++ ":  Generating Multstar 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (*) hf res2))) allMaps
    let zipped5 = trace (name ++ ":  Generating AverageStar 0/" ++ show size) $ zip (repeat (aStar2 res2 (algorithmFunction (\x y -> ((x + y) `div` 2) ^ 2) hf res2))) allMaps
    lst  <- foldl benchOnMap initBench zipped
    lst2 <- foldl benchOnMap initBench zipped2
    lst3 <- foldl benchOnMap initBench zipped3
    lst4 <- foldl benchOnMap initBench zipped4
    lst5 <- foldl benchOnMap initBench zipped5
    let newLst1 = ("AStar", map (\(_, _, _, _, t, c) -> (t, c)) lst)
    let newLst2 = ("AStar3", map (\(_, _, _, _, t, c) -> (t, c)) lst2)
    let newLst3 = ("AStar8", map (\(_, _, _, _, t, c) -> (t, c)) lst3)
    let newLst4 = ("MultStar", map (\(_, _, _, _, t, c) -> (t, c)) lst4)
    let newLst5 = ("AverageStar", map (\(_, _, _, _, t, c) -> (t, c)) lst5)
    -- [timeComplexity, MemoryComplexity, Strokes, BranchDepth, Time, complexity], acF
    let ret = oui2 [newLst1, newLst2, newLst3, newLst4, newLst5]
    writeFile name $ "[\n" ++ toJson2 ret ++ "]"
    -- writeFile name $ "[\n" ++ toJsonV2 ret ++ "]"

makeDepthGraph :: Heuristic -> String -> IO()
makeDepthGraph hf name = do
    (_, res) <- parse "MapSolved/Map3x3"
    (_, grill) <- parse "test-files/valids/01-map"
    let (moves, acts, _, _) = aStarBench grill res (algorithmFunction (+) hf res)
    let newActs = Actions.insert (foldl (\tree act -> Actions.insert tree act False) Actions.new acts) moves True
    writeFile name (show newActs)

regenerate :: IO ()
regenerate = do
    makeBarChart manhattan "docs/manhattan/bar.json" 100
    makeDepthGraph manhattan "docs/manhattan/depth.json"
    makeLineChart manhattan "docs/manhattan/line.json" 50
    makeBarChart (wManhattan 3) "docs/manhattan3/bar.json" 100
    makeDepthGraph (wManhattan 3) "docs/manhattan3/depth.json"
    makeLineChart (wManhattan 3) "docs/manhattan3/line.json" 50
    makeBarChart (wManhattan 8) "docs/manhattan8/bar.json" 100
    makeDepthGraph (wManhattan 8) "docs/manhattan8/depth.json"
    makeLineChart (wManhattan 8) "docs/manhattan8/line.json" 50
    makeBarChart euclidean "docs/euclidean/bar.json" 100
    makeDepthGraph euclidean "docs/euclidean/depth.json"
    makeLineChart euclidean "docs/euclidean/line.json" 50
    makeBarChart dijkstra "docs/dijkstra/bar.json" 10
    makeDepthGraph dijkstra "docs/dijkstra/depth.json"
    makeLineChart dijkstra "docs/dijkstra/line.json" 10
    makeAverage (wManhattan 99)
    exitWith ExitSuccess

benchmark :: IO ()
benchmark = do
    args <- getArgs
    checkHFlag args
    checkRFlag args
    (_, grill) <- parse "test-files/valids/map-txt"
    (_, grill2) <- parse "test-files/valids/01-map"
    (_, res) <- parse "MapSolved/Map4x4"
    (_, res2) <- parse "MapSolved/Map3x3"
    checkGrill grill res
    lst1 <- foldl calcBench initBench (createData algo (init heuristic) grill res "map-txt")
    lst2 <- foldl calcBench initBench (createData algo heuristic grill2 res2 "01-map")
    let lst = lst1 ++ lst2
    let chart = getVFlag args
    putStrLn "Time Complexity:"
    printBench $ map (\(name, x, _, _, _) -> (name, x)) $ sortOn (\(_, x, _, _, _) -> x) lst
    putStrLn "\nMemory Complexity:"
    printBench $ map (\(name, _, x, _, _) -> (name, x)) $ sortOn (\(_, _, x, _, _) -> x) lst
    putStrLn "\nNumber of moves:"
    printBench $ map (\(name, _, _, x, _) -> (name, x)) $ sortOn (\(_, _, _, x, _) -> x) lst
    putStrLn "\nTime:"
    printBench $ map (\(name, _, _, _, x) -> (name, x)) $ sortOn (\(_, _, _, _, x) -> x) lst
    printChart $ splitOn "+" chart
    exitWith ExitSuccess
        where
            printChart :: [String] -> IO()
            printChart [] = return ()
            printChart (x:[])
                | x == "dijkstra" = do
                    makeDepthGraph dijkstra "docs/dijkstra/depth.json"
                    makeBarChart dijkstra "docs/dijkstra/bar.json" 10
                    makeLineChart dijkstra "docs/dijkstra/line.json" 10
                | x == "euclidean" = do
                    makeBarChart euclidean "docs/euclidean/bar.json" 100
                    makeDepthGraph euclidean "docs/euclidean/depth.json"
                    makeLineChart euclidean "docs/euclidean/line.json" 50
                | x == "manhattan" = do
                    makeBarChart (wManhattan 1) "docs/manhattan/bar.json" 100
                    makeDepthGraph (wManhattan 1) "docs/manhattan/depth.json"
                    makeLineChart (wManhattan 1) "docs/manhattan/line.json" 50
                | otherwise = return ()
            printChart (x:x1:xs)
                | x == "dijkstra" = do
                    makeDepthGraph dijkstra "docs/dijkstra/depth.json"
                    makeBarChart dijkstra "docs/dijkstra/bar.json" 10
                    makeLineChart dijkstra "docs/dijkstra/line.json" 10
                    printChart $ x1:xs
                | x == "euclidean" && length sec /= length x1 = do
                    makeBarChart euclidean "docs/euclidean/bar.json" 100
                    makeDepthGraph euclidean "docs/euclidean/depth.json"
                    makeLineChart euclidean "docs/euclidean/line.json" 50
                    printChart $ x1:xs
                | x == "euclidean" = do
                    let nb = read sec :: Int
                    makeBarChart (wEuclidean nb) "docs/tmp/bar.json" 100
                    makeDepthGraph (wEuclidean nb) "docs/tmp/depth.json"
                    makeLineChart (wEuclidean nb) "docs/tmp/line.json" 50
                    makeAverage (wEuclidean nb)
                    printChart xs
                | x == "manhattan" && length sec /= length x1 = do
                    makeBarChart (wManhattan 1) "docs/manhattan/bar.json" 100
                    makeDepthGraph (wManhattan 1) "docs/manhattan/depth.json"
                    makeLineChart (wManhattan 1) "docs/manhattan/line.json" 50
                    printChart $ x1:xs
                | x == "manhattan" = do
                    let nb = read sec :: Int
                    makeBarChart (wManhattan nb) "docs/tmp/bar.json" 100
                    makeDepthGraph (wManhattan nb) "docs/tmp/depth.json"
                    makeLineChart (wManhattan nb) "docs/tmp/line.json" 50
                    makeAverage (wManhattan nb)
                    printChart xs
                | otherwise = trace("Error Parsing: " ++ x) $ printChart $ x1:xs
                    where
                        sec = filter (\x -> x `elem` ['0'..'9']) x1

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