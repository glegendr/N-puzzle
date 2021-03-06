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
import Debug.Trace
import Actions
import Data.List.Split
import Data.Char
import Generate (generateMapList)
import Utils

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
    putStrLn $ "-v --visual <value>\t\t   Allow you to change site datas:\n\t\t\t\t   Example: ./" ++ name ++ " -b -v=\"dijkstra+wEuclidean->4+manhattan\"\n\t\t\t\t   A new algorithm will generate a new page in the site with all datas"
    putStrLn $ "-r --regenerate\t\t\t   Regenerate all site data"
    putStrLn $ "-h --help\t\t\t   Display this message"
    exitWith ExitSuccess

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
    printf "%-15s%-15s%-8s -> " n1 n2 n3
    print x
    printBench xs

av :: Double -> (Double, Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double, Double)
av div (a, b, c, d, e, f) = (a / div, b / div, c / div, d / div, e / div, f / div)

tuppleToList :: (a, a, a, a, a, a) -> [a]
tuppleToList (a, b, c, d, e, f) = [a, b, c, d, e, f]

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
    where
        newLst = map (\(_, tupple) -> tupple) lst
        allMax = foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (max acA a, max acB b, max acC c, max acD d, max acE e, max acF f)) newLst
        zipAll :: [String] -> [[(String, String)]] -> (String, (String, String, String, String, String, String)) -> [[(String, String)]]
        zipAll strs acc (name, tupple) = (zip strs $ name : (tuppleToList tupple)) : acc

toJsonV2 :: [(String, [[(String, String)]])] -> String
toJsonV2 [] = []
toJsonV2 ((name, lst):[]) = "{\n\"name\": \"" ++ name ++ "\",\n\"values\": [\n" ++ toJson2 lst ++ "]\n}\n"
toJsonV2 ((name, lst):xs) = "{\n\"name\": \"" ++ name ++ "\",\n\"values\": [\n" ++ toJson2 lst ++ "]\n},\n" ++ toJsonV2 xs 

generateAverageList :: Heuristic -> Int -> [Grill] -> String -> Grill -> IO [(Double, Double, Double, Double, Double, Double)]
generateAverageList hf size allMaps name res2 = do
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
    let av1 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst
    let av2 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst2
    let av3 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst3
    let av4 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst4
    let av5 = av (realToFrac $ length allMaps) $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst5
    return [av1, av2, av3, av4, av5]

generateList :: [(Double, Double, Double, Double, Double, Double)] -> Heuristic -> String -> Int -> IO [(Double, Double, Double, Double, Double, Double)]
generateList lst _ _ _
    | length lst == 6 = return lst
generateList _ hf name size = do
    (_, res2) <- parse "MapSolved/Map3x3"
    let allMaps = generateMapList 3 size res2
    lst <- trace ("Manhattan:") $ generateAverageList manhattan size allMaps name res2
    let av1 = av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst
    lst2 <- trace ("Manhattan3:") $ generateAverageList (wManhattan 3) size allMaps name res2
    let av2 = av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst2
    lst3 <- trace ("Manhattan8:") $ generateAverageList (wManhattan 8) size allMaps name res2
    let av3 = av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst3
    lst4 <- trace ("Euclidean:") $ generateAverageList euclidean size allMaps name res2
    let av4 = av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst4
    lst5 <- trace ("Dijkstra:") $ generateAverageList dijkstra size allMaps name res2
    let av5 = av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst5
    lst6 <- trace ("Temporary:") $ generateAverageList hf size allMaps name res2
    let av6 = av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f)) lst6
    return [av1, av2, av3, av4, av5, av6]

makeAverage :: Heuristic -> [(Double, Double, Double, Double, Double, Double)] -> Int -> IO ()
makeAverage hf lst size = do
    let name = "docs/data.json"
    newLst <- generateList lst hf name size
    toJson name ["name", "timeComplexity", "memoryComplexity", "strokes", "branchDepth", "time", "complexity"] [("Manhattan", newLst !! 0), ("Manhattan3", newLst !! 1), ("Manhattan8", newLst !! 2), ("Euclidean", newLst !! 3), ("Dijkstra", newLst !! 4), ("TemporaryValue", newLst !! 5)]


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
    let allMaps = generateMapList 3 size res2
    avLst <- generateAverageList hf size allMaps name res2 
    let ret = oui ["TimeComplexity", "MemoryComplexity", "Strokes", "BranchDepth", "Time", "Complexity"] avLst
    writeFile name $ "[\n" ++ toJsonV2 ret ++ "]"

oui2 :: [(String, [(Double, Double)])] -> [[(String, String)]]
oui2 [] = []
oui2 ((name, lst):xs) = [("name", name), ("timeScope", show timeList), ("complexityScope", show compList)] : oui2 xs
    where
        timeList = map (\(time, _) -> time) lst
        compList = map (\(_, comp) -> comp) lst


makeDepthGraph :: Heuristic -> String -> IO()
makeDepthGraph hf name = do
    (_, res) <- parse "MapSolved/Map3x3"
    (_, grill) <- parse "test-files/valids/01-map"
    let (moves, acts, _, _) = aStarBench grill res (algorithmFunction (+) hf res)
    let newActs = Actions.insert (foldl (\tree act -> Actions.insert tree act False) Actions.new acts) moves True
    writeFile name (show newActs)

makeChart :: Heuristic -> String -> Int -> IO (Double, Double, Double, Double, Double, Double)
makeChart hf name size = do
    makeDepthGraph hf (name ++ "depth.json")
    (_, res2) <- parse "MapSolved/Map3x3"
    let allMaps = generateMapList 3 size res2
    avLst <- generateAverageList hf size allMaps name res2
    let ret2 = oui ["TimeComplexity", "MemoryComplexity", "Strokes", "BranchDepth", "Time", "Complexity"] avLst
    writeFile (name ++ "bar.json") $ "[\n" ++ toJsonV2 ret2 ++ "]"
    return (av 5 $ foldl1 (\(acA, acB, acC, acD, acE, acF) (a, b, c, d, e, f) -> (acA + a, acB + b, acC + c, acD + d, acE + e, acF + f))  avLst)

regenerate :: IO ()
regenerate = do
    z5 <- makeChart dijkstra "docs/dijkstra/" 40
    z1 <- makeChart manhattan "docs/manhattan/" 40
    z2 <- makeChart (wManhattan 3) "docs/manhattan3/" 40
    z3 <- makeChart (wManhattan 8) "docs/manhattan8/" 40
    z4 <- makeChart euclidean "docs/euclidean/" 40
    z6 <- makeChart (wManhattan 99) "docs/tmp/" 40
    makeAverage (wManhattan 99) [z1, z2, z3, z4, z5, z6] 40
    exitWith ExitSuccess

benchmark :: IO ()
benchmark = do
    args <- getArgs
    checkHFlag args helper
    checkRFlag args
    (_, grill) <- parse "test-files/valids/map-txt"
    (_, grill2) <- parse "test-files/valids/01-map"
    (_, res) <- parse "MapSolved/Map4x4"
    (_, res2) <- parse "MapSolved/Map3x3"
    checkGrill grill res
    checkGrill grill2 res2
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
            printChart (x:xs)
                | str == "dijkstra" = do
                    makeChart dijkstra "docs/dijkstra/" 40
                    printChart xs
                | str == "euclidean" = do
                    makeChart euclidean "docs/euclidean/" 40
                    printChart xs
                | (head algoArrow) == "weuclidean" = do
                    let nb = (checkWeight $ tail algoArrow)
                    if (nb == 1)
                    then makeChart euclidean "docs/euclidean/" 40
                    else (
                        do
                            makeAverage (wEuclidean nb) [] 1
                            makeChart (wEuclidean nb) "docs/tmp/" 40)
                    printChart xs
                | str == "manhattan"= do
                    makeChart (wManhattan 1) "docs/manhattan/" 40
                    printChart xs
                | (head algoArrow) == "wmanhattan" = do
                    let nb = (checkWeight $ tail algoArrow)
                    if (nb == 3)
                    then makeChart (wManhattan nb) "docs/manhattan3/" 40
                    else if (nb == 8)
                    then makeChart (wManhattan nb) "docs/manhattan8/" 40
                    else if (nb == 1)
                    then makeChart manhattan "docs/manhattan/" 40
                    else (
                        do
                            makeAverage (wManhattan nb) [] 40
                            makeChart (wManhattan nb) "docs/tmp/" 40)
                    printChart xs
                | otherwise = trace("Error Parsing: " ++ x) $ printChart xs
                    where
                        str = map toLower x
                        algoArrow = splitOn "->" str

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

initBench = return []