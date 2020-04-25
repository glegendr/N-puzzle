module Generate
(generateMapList
, generate) where

import Grill
import Parsing
import Data.List as Dl
import Data.ByteString as Bs (hGetLine, filter, elem, ByteString)
import System.IO
import Data.Hashable
import System.Exit
import Utils
import Data.List.Split
import System.Environment


generateMapList :: Int -> Int -> Grill -> [Grill]
generateMapList size number res =  take number $  Dl.filter (checkGrillBool res) $ map toGrill $ permutations [0..(size^2 - 1)]

generateMapSalt :: Bool -> Grill -> IO Grill
generateMapSalt solvable res = do
    handle <- openFile "/dev/random" ReadMode  
    lines <- Bs.hGetLine handle
    hClose handle
    let size = length res
    let salt = hash lines
    let saltedGrill = generateIntGrill salt size
    if (checkGrillBool res saltedGrill == solvable)
    then return saltedGrill
    else generateMapSalt solvable res

generateIntGrill :: Int -> Int -> Grill
generateIntGrill salt size =
    newGrillList (nub $ (divSalt salt size ++ [0..(size ^ 2 - 1)])) size
    where
        divSalt :: Int -> Int -> [Int]
        divSalt salt _
            | salt < 10 && salt > -10 = []
        divSalt salt size = salt `mod` divBy : divSalt (salt `div` 2) size
            where divBy = (size ^ 2)

getSFlag :: [String] -> Int
getSFlag [] = 3
getSFlag (x:x1:xs)
    | x == "-s" || x == "--size" = checkWeight [x1]
    | head eq == "-s" || head eq == "--size" = checkWeight $ tail eq
    | otherwise = getSFlag $ x1:xs
    where eq = splitOn "=" x
getSFlag (x:xs)
    | x == "-s" || x == "--size" = error $ "No number provided after " ++ x ++ " flag"
    | head eq == "-s" || head eq == "--size" = checkWeight $ tail eq
    | otherwise = getSFlag xs
    where eq = splitOn "=" x

getBFlag :: [String] -> Bool
getBFlag [] = True
getBFlag (x:x1:xs)
    | x == "-b" || x == "--bool" = checkBool [x1]
    | head eq == "-b" || head eq == "--bool" = checkBool $ tail eq
    | otherwise = getBFlag $ x1:xs
    where eq = splitOn "=" x
getBFlag (x:xs)
    | x == "-b" || x == "--bool" = error $ "No Bool provided after " ++ x ++ " flag"
    | head eq == "-b" || head eq == "--bool" = checkBool $ tail eq
    | otherwise = getBFlag xs
    where eq = splitOn "=" x

getOFlag :: [String] -> String
getOFlag [] = ""
getOFlag (x:x1:xs)
    | x == "-o" || x == "--output" = checkString [x1]
    | head eq == "-o" || head eq == "--output" = checkString $ tail eq
    | otherwise = getOFlag $ x1:xs
    where eq = splitOn "=" x
getOFlag (x:xs)
    | x == "-o" || x == "--output" = error $ "No Path provided after " ++ x ++ " flag"
    | head eq == "-o" || head eq == "--output" = checkString $ tail eq
    | otherwise = getOFlag xs
    where eq = splitOn "=" x

helper :: IO ()
helper = do
    name <- getProgName
    putStrLn $ "Example: ./" ++ name ++ " -g -s=<value> -b=<value> -o=<path> -r=<path>\n"
    putStrLn $ "-s --size   <value>\t\t   Change puzzle's size"
    putStrLn $ "-b --bool   <value>\t\t   Change puzzle's solvability:\n\t\t\t\t   <false>/<0> <true>/<1>"
    putStrLn $ "-o --output <path>\t\t   Change puzzle's output"
    putStrLn $ "-r --result <map>\t\t   Allow you to choose a puzzle with which the generated puzzle will work"
    putStrLn $ "-h --help\t\t\t   Display this message"
    exitWith ExitSuccess

generate :: IO ()
generate = do
    args <- getArgs
    checkHFlag args helper
    let size = getSFlag args
    (_, res) <- getRFlag args size
    let bool = getBFlag args
    let path = getOFlag args
    randLst <- generateMapSalt bool res
    let ret = "# This puzzle is " ++ isSolvable bool ++ "\n" ++ formatFileGrill randLst
    if (path == "")
    then putStr ret
    else writeFile path ret
    exitWith ExitSuccess
    where
        isSolvable :: Bool -> String
        isSolvable x
            | x == True = "solvable"
            | otherwise = "unsolvable"