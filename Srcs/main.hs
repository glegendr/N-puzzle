import Srcs.Parsing
import System.Environment
import Data.Char
import Srcs.Grill
import Text.Printf
import System.Exit

flags = [("-m", "--map", "<map>\t      /!\\  Mandatory flag: map to solve"), ("-r", "--result", "<map>\t\t   Allow you to give a result map"), ("-f", "--function", "<function name>\t   Allow you to change your heuristic function"), ("-h", "--help", "\t\t\t   Display this message")]

getMFlag :: [String] -> IO (Int, Grill)
getMFlag [] = error "No map provided"
getMFlag (x:x1:xs)
    | x == "-m" || x == "--map" = parse x1
    | otherwise = getMFlag $ x1:xs
getMFlag (x:xs) = error "No map provided"

getRFlag :: [String] -> Int -> IO (Int, Grill)
getRFlag [] size = parse $ "MapSolved/Map" ++ (show size) ++ "x" ++ (show size)
getRFlag (x:x1:xs) size 
    | x == "-r" || x == "--result" = parse x1
    | otherwise = getRFlag (x1:xs) size 
getRFlag (x:xs) size 
    | x == "-r" || x == "--result" = error $ "No map provided after " ++ x ++ " flag"
    | otherwise = getRFlag xs size

getFFlag :: [String] -> (Grill -> [(Int, Int)])
getFFlag [] = manathan
getFFlag (x:x1:xs)
    | x == "-f" || x == "--function" = dispatch [x1]
    | otherwise = getFFlag $ x1:xs
getFFlag (x:xs)
    | x == "-f" || x == "--function" = error $ "No function provided after " ++ x ++ " flag"
    | otherwise = getFFlag xs

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
    | otherwise = error $ "Flag " ++ x ++ " doesn't exist"
checkFlags (x:xs)
    | x == "-h" || x == "--help" = helper
    | x `elem` (foldl (\acc (fs, sc, _) -> acc ++ [fs, sc]) [] flags) = error $ "Something is needded after " ++ x ++ " flag"
    | otherwise = error $ "Flag " ++ x ++ " doesn't exist"


main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    ((size, grill), (size2, res), _) <- leakser args
    if size /= size2
    then error "Not same size between given map and result map"
    else return ()
    printGrill grill
    putStrLn "--------------"
    printGrill res

dispatch :: [String] -> (Grill -> [(Int, Int)])
dispatch [] = manathan
dispatch x
    | str == "manathan" = manathan
    | str == "1" = un
    | str == "2" = deux
    | otherwise = error $ "action \"" ++ (head x) ++ "\" not found"
    where str = map toLower $ head x

manathan :: Grill -> [(Int, Int)]
manathan [] = []
manathan (x:xs) = (head x, 0) : manathan xs

un :: Grill -> [(Int, Int)]
un [] = []
un (x:xs) = (head x, 0) : un xs

deux:: Grill -> [(Int, Int)]
deux [] = []
deux (x:xs) = (head x, 0) : deux xs