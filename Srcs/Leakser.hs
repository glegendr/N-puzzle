module Srcs.Leakser
(leakser, checkFlags) where

import Srcs.Heuristic
import Srcs.Grill (Grill)
import Srcs.Parsing
import Text.Printf
import System.Exit
import Data.List.Split
import Data.Char

flags = [("-m", "--map", "<map>\t      /!\\  Mandatory flag: map to solve"), ("-r", "--result", "<map>\t\t   Allow you to give a result map"), ("-f", "--function", "<function name>\t   Allow you to change your heuristic function"), ("-h", "--help", "\t\t\t   Display this message")]

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

getFFlag :: [String] -> (Grill -> [(Int, Int)])
getFFlag [] = manathan
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

leakser :: [String] -> IO ((Int, Grill), (Int, Grill), (Grill -> [(Int, Int)]))
leakser lst = do
    (size, m) <- getMFlag lst
    r <- getRFlag lst size
    return ((size, m), r, getFFlag lst)
 
helper :: IO ()
helper = do
    let x = map (\(fst, sec, third) -> fst ++ sec ++ third) flags
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


dispatch :: String -> (Grill -> [(Int, Int)])
dispatch [] = manathan
dispatch x
    | str == "manathan" = manathan
    | str == "1" = un
    | str == "2" = deux
    | otherwise = error $ "action \"" ++ x ++ "\" not found"
    where str = map toLower x

