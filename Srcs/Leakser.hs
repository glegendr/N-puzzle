module Srcs.Leakser
(leakser, checkFlags) where

import Srcs.Heuristic
import Srcs.Grill (Grill)
import Srcs.Parsing
import Text.Printf
import System.Exit
import Data.List.Split
import Data.Char
import System.Environment

flags = [
    ("-m", "--map", "<map>\t      /!\\  Mandatory flag: map to solve")
    ,("-r", "--result", "<map>\t\t   Allow you to give a result map")
    ,("-f", "--function", "<function name>\t   Allow you to change your heuristic function as:\n\t\t\t\t   <manhattan> <wManhattan->weight> <euclidean> <wEuclidean->weight> <dijkstra>")
    , ("-a", "--algorithm", "<algorythm name>\t   Allow you to change your search function as:\n\t\t\t\t   <aStar> <wAStar->weight> <minimizedAStar> <multstar>")
    , ("-v", "--visual", "<value>\t\t   Print all N-puzzle's steps as:\n\t\t\t\t   <empty>/<0> <parcial>/<1> <all>/<2>")
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

getRFlag :: [String] -> Int -> IO (Int, Grill)
getRFlag [] size = parse $ "MapSolved/Map" ++ (show size) ++ "x" ++ (show size)
getRFlag (x:x1:xs) size 
    | x == "-r" || x == "--result" = parse x1
    | head eq == "-r" || head eq == "--result" = parse $ last eq
    | otherwise = getRFlag (x1:xs) size
    where eq = splitOn "=" x
getRFlag (x:xs) size
    | x == "-r" || x == "--result" = error $ "No map provided after " ++ x ++ " flag"
    | head eq == "-r" || head eq == "--result" = parse $ last eq
    | otherwise = getRFlag xs size
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


getVFlag :: [String] -> String
getVFlag [] = "partial"
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

leakser :: [String] -> IO (Grill, Grill, Heuristic, (Int -> Int -> Int), String)
leakser lst = do
    (size, m) <- getMFlag lst
    (size2, r) <- getRFlag lst size
    if size /= size2
    then  error "Not same size between given map and result map"
    else return (m, r, getFFlag lst, getAFlag lst, getVFlag lst)
 
helper :: IO ()
helper = do
    let x = map (\(fst, sec, third) -> fst ++ sec ++ third) flags
    name <- getProgName
    putStrLn $ "Example: " ++ name ++ " -m=<map> -r=<map> -a=<algorithm> -f=<function> -v=<value>\n"
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
    | x `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = checkFlags xs
    | head eq `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = checkFlags $ x1:xs
    | otherwise = error $ "Flag " ++ x ++ " doesn't exist"
    where eq = splitOn "=" x
checkFlags (x:xs)
    | x == "-h" || x == "--help" = helper
    | x `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = error $ "Something is needded after " ++ x ++ " flag"
    | head eq `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = checkFlags xs
    | otherwise = error $ "Flag " ++ x ++ " doesn't exist"
    where eq = splitOn "=" x

checkWeight :: [String] -> Int
checkWeight [] = 1
checkWeight (x:x1:xs) = error "multiple equal"
checkWeight (x:xs)
    | all isDigit x == True = read x :: Int 
    | otherwise = error $ "wrong weight found: " ++ x

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
    | otherwise = error $ "algorithm \"" ++ x ++ "\" not found"
    where
    str = map toLower x
    algoArrow = splitOn "->" str
        