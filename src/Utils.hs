module Utils
(checkWeight, checkBool, getRFlag, checkHFlag, checkString) where

import Data.Char
import Parsing
import Data.List.Split

checkWeight :: [String] -> Int
checkWeight [] = 1
checkWeight (x:[])
    | all isDigit x == True = read x :: Int 
    | otherwise = error $ "wrong weight found: " ++ x
checkWeight _ = error "multiple equal"

checkString :: [String] -> String
checkString [] = ""
checkString (x:[]) = x
checkString _ = error "multiple equal"


checkBool :: [String] -> Bool
checkBool [] = True
checkBool (x:[])
    | str == "true" || str == "1" = True
    | str == "false" || str == "0" = False
    | otherwise = error $ "wrong boolean found: " ++ x
    where str = map toLower x
checkBool _ = error "multiple equal"

getRFlag :: [String] -> Int -> IO (Int, [[Int]])
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

checkHFlag :: [String] -> IO () -> IO ()
checkHFlag [] _ = return ()
checkHFlag (x:xs) f
    | x == "-h" || x == "--help" = f
    | otherwise = checkHFlag xs f