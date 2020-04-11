module Srcs.Parsing
( checkGrill
, parse) where

import System.IO
import System.Environment
import Data.List
import Data.Char
import Srcs.Grill (Grill)

inversion :: [Int] -> Int
inversion [] = 0
inversion (x:xs) = foldl (\acc y -> if y < x && y /= 0 then acc + 1 else acc) 0 xs + inversion xs

checkGrill :: Grill -> Grill -> IO ()
checkGrill grill res
    | odd size == True && even grillInv == even resInv = return ()
    | even size == True && even (grillInv + posZeroGrill `div` size) == even (resInv + posZeroRes `div` size) = return ()
    | otherwise= error "Map is unsolvable"
    where
        size = length grill
        newGrill = foldl1 (++) grill
        newRes = foldl1 (++) res
        grillInv = inversion newGrill
        resInv = inversion newRes
        (Just posZeroGrill) = elemIndex 0 newGrill
        (Just posZeroRes) = elemIndex 0 newRes

deleteComment :: [String] -> [String]
deleteComment [] = []
deleteComment (x:xs)
    | x == [] = error "empty line"
    | noCom == [] = deleteComment xs
    | otherwise = noCom : deleteComment xs
    where noCom = takeWhile (/= '#') x

checkAlpha :: [String] -> IO ()
checkAlpha [] = return ()
checkAlpha (x:xs)
    | nlyAlpha /= "" = error $ "Alpha found: " ++ (show $ head nlyAlpha)
    | otherwise = checkAlpha xs
    where nlyAlpha = filter (isAlpha) x

checkLinesSize :: [[String]] -> Int -> Bool
checkLinesSize [] _ = False
checkLinesSize (x:xs) size
    | length x /= size = True
    | otherwise = checkLinesSize xs size

checkSize :: [[String]] -> IO ()
checkSize [] = error "Empty map"
checkSize lst
    | size < 3 = error "map to small: expected 3 or more"
    | (tail $ head lst) /= [] = error "Wrong size given"
    | length lst /= 1 + size = error $ "Wrong number of line: expected " ++ show size
    | checkLinesSize (tail lst) size == True =  error $ "Wrong line's size: expected " ++ show size
    | otherwise = return ()
    where size = (read $ head $ head lst)

checkDuplicate :: [String] -> IO ()
checkDuplicate [] = return ()
checkDuplicate lst
    | length (nub $ newlst) /= length newlst = error "Duplicate symbols"
    | otherwise = return ()
    where newlst = words $ unlines $ tail lst

checkOrdered :: [Int] -> Int -> IO ()
checkOrdered [] _ = return ()
checkOrdered (x:xs) bef
    | x == bef + 1 = checkOrdered xs x
    | otherwise = error $ "Wrong symbol founded: " ++ show x

parse :: String -> IO (Int, Grill)
parse file =  do
    content <- readFile file
    let grill = deleteComment $ lines content
    checkAlpha grill
    checkSize $ map words grill
    checkDuplicate $ words $ unlines grill
    checkOrdered (sort $ map (\x -> read x ::Int) $ tail $ words $ unlines grill) (-1)
    return (read (head grill)::Int, map (map (\x -> read x ::Int)) $ tail $ map words grill)