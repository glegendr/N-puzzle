module Leakser
(leakser, checkFlags, helper) where

import Heuristic
import Grill (Grill)
import Parsing
import Benchmark
import Text.Printf
import System.Exit
import Data.List.Split
import Data.Char
import System.Environment
import Generate
import Utils
import Actions

flags = [
    ("-m", "--map", " <map>\t      /!\\  Mandatory flag: map to solve")
    ,("-r", "--result", " <map>\t\t   Allow you to give a result map")
    ,("-f", "--function", " <function name>\t   Allow you to change your heuristic function as:\n\t\t\t\t   <manhattan> <wManhattan->weight> <euclidean> <wEuclidean->weight> <dijkstra>")
    ,("-a", "--algorithm", "<algorythm name>\t   Allow you to change your search function as:\n\t\t\t\t   <aStar> <wAStar->weight> <minimizedAStar> <multStar> <averageStar>")
    ,("-v", "--visual", " <value>\t\t   Print steps:\n\t\t\t\t   <empty>/<0> <partial>/<1> <all>/<2> <animated>/<3>")
    ,("-i", "--inverse", " <moves>\t\t   Reverse the process, you now give a list of moves, and we tell if the puzzle is sorted or not")
    ,("-b", "--benchmark","\t\t\t   Launch benchmarks. Try -b -h to get help on benchmarks")
    ,("-g", "--generate", "\t\t\t   Launch puzzle generator. Try -g -h to get help on map generator")
    ,("-h", "--help", "\t\t\t   Display this message")]

getMFlag :: [String] -> IO (Int, Grill)
getMFlag [] = error "No map provided"
getMFlag (x:x1:xs)
    | x == "-m" || x == "--map" = parse x1
    | head eq == "-m" || head eq == "--map" = parse $ last eq
    | otherwise = getMFlag $ x1:xs
    where eq = splitOn "=" x
getMFlag (x:xs)
    | head eq == "-m" || head eq == "--map" = parse $ last eq
    | otherwise = error "No map provided"
    where eq = splitOn "=" x

getFFlag :: [String] -> Heuristic
getFFlag [] = manhattan
getFFlag (x:x1:xs)
    | x == "-f" || x == "--function" = dispatch x1
    | head eq == "-f" || head eq == "--function" = dispatch $ last eq
    | otherwise = getFFlag $ x1:xs
    where eq = splitOn "=" x
getFFlag (x:xs)
    | x == "-f" || x == "--function" = error $ "No function provided after " ++ x ++ " flag"
    | head eq == "-f" || head eq == "--function" = dispatch $ last eq
    | otherwise = getFFlag xs
    where eq = splitOn "=" x
    
getAFlag :: [String] -> (Int -> Int -> Int)
getAFlag [] = (+)
getAFlag (x:x1:xs) 
    | x == "-a" || x == "--algorithm" = dispatchAlgo x1
    | head eq == "-a" || head eq == "--algorithm" = dispatchAlgo $ last eq
    | otherwise = getAFlag (x1:xs)
    where eq = splitOn "=" x
getAFlag (x:xs)
    | x == "-a" || x == "--algorithm" = error $ "No function provided after " ++ x ++ " flag"
    | head eq == "-a" || head eq == "--algorithm" = dispatchAlgo $ last eq
    | otherwise = getAFlag xs
    where eq = splitOn "=" x

getIFlag :: [String] -> [Act]
getIFlag [] = []
getIFlag (x:x1:xs) 
    | x == "-i" || x == "--inverse" = checkActs [x1]
    | head eq == "-i" || head eq == "--inverse" = checkActs $ tail eq
    | otherwise = getIFlag (x1:xs)
    where eq = splitOn "=" x
getIFlag (x:xs)
    | x == "-i" || x == "--inverse" = error $ "No List provided after " ++ x ++ " flag"
    | head eq == "-i" || head eq == "--inverse" = checkActs $ tail eq
    | otherwise = getIFlag xs
    where eq = splitOn "=" x

leakser :: [String] -> IO (Grill, Grill, Heuristic, (Int -> Int -> Int), String, [Act])
leakser lst = do
    (size, m) <- getMFlag lst
    (size2, r) <- getRFlag lst size
    if size /= size2
    then  error "Not same size between given map and result map"
    else return (m, r, getFFlag lst, getAFlag lst, getVFlag lst, getIFlag lst)
 
helper :: IO ()
helper = do
    let x = map (\(fst, sec, third) -> fst ++ sec ++ third) flags
    name <- getProgName
    putStrLn $ "Example: ./" ++ name ++ " -m=<map> -r=<map> -a=<algorithm> -f=<function> -v=<value>\n"
    printHelp flags
    exitWith ExitSuccess
    where
        printHelp :: [(String, String, String)] -> IO ()
        printHelp [] = return ()
        printHelp ((fst, sec, third):xs) = do
            printf "%s %-10s %s\n" fst sec third
            printHelp xs

checkFlags :: [String] -> IO ()
checkFlags [] = return ()
checkFlags (x:x1:xs)
    | x == "-h" || x == "--help" = helper
    | x == "-b" || x == "--benchmark" = benchmark
    | x == "-g" || x == "--generate" = generate
    | x `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = checkFlags xs
    | head eq `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = checkFlags $ x1:xs
    | otherwise = error $ "Flag " ++ x ++ " doesn't exist"
    where eq = splitOn "=" x
checkFlags (x:xs)
    | x == "-h" || x == "--help" = helper
    | x == "-b" || x == "--benchmark" = benchmark
    | x == "-g" || x == "--generate" = generate
    | x `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = error $ "Something is needded after " ++ x ++ " flag"
    | head eq `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = checkFlags xs
    | otherwise = error $ "Flag " ++ x ++ " doesn't exist"
    where eq = splitOn "=" x

dispatch :: String -> Heuristic
dispatch [] = manhattan
dispatch x
    | str == "manhattan" = manhattan
    | (head wman) == "wmanhattan" = wManhattan $ checkWeight $ tail wman
    | str == "euclidean" = euclidean
    | (head wman) == "weuclidean" = wEuclidean $ checkWeight $ tail wman
    | str == "dijkstra" = dijkstra
    | otherwise = error $ "action \"" ++ x ++ "\" not found"
    where
        str = map toLower x
        wman = splitOn "->" str

dispatchAlgo :: String -> (Int -> Int -> Int)
dispatchAlgo [] = (+)
dispatchAlgo x
    | str == "astar" = (+)
    | (head algoArrow) == "wastar" = (\x y -> x + (checkWeight $ tail algoArrow) * y)
    | str == "multstar" = (*)
    | str == "minimizedastar" = (-)
    | str == "averagestar" = (\x y -> ((x + y) `div` 2) ^ 2)
    | otherwise = error $ "algorithm \"" ++ x ++ "\" not found"
    where
    str = map toLower x
    algoArrow = splitOn "->" str
        
