module Grill
( Grill
, moveRight
, moveLeft
, moveUp
, moveDown
, moveAct
, printGrill
, printGrillRes
, printFileGrill
, formatFileGrill
, newGrill
, newGrillList
, toGrill
) where

import Rainbow
import Data.Function ((&))
import Data.List
import Actions
import Data.Text (pack)

type Grill = [[Int]]

printMe :: [Int] -> IO ()
printMe [] = putStrLn "|"
printMe (x:xs)
    | x == 0 = do
        putStr "|    "
        printMe xs
    | x < 10 = do
        putStr $ "|  " ++ show x ++ " "
        printMe xs
    | otherwise = do
        putStr $ "| " ++ show x ++ " "
        printMe xs

printGrill :: Grill -> IO ()
printGrill [] =  return ()
printGrill (x:xs) = do
    putStrLn str
    printMe x
    if xs == []
    then putStrLn str
    else return ()
    printGrill xs
    where str = foldl (\acc _  -> acc ++ "----+") "+" x

printFileGrill :: Grill -> IO ()
printFileGrill grill = putStr $ formatFileGrill grill

formatFileGrill :: Grill -> String
formatFileGrill [] = []
formatFileGrill grill = show size ++ "\n" ++ formatGrill grill
    where
        size = length grill
        formatGrill :: Grill -> String
        formatGrill [] = []
        formatGrill (x:xs) = foldl (\acc y -> acc ++ show y ++ " ") [] x ++ "\n" ++ formatGrill xs 

printMeRes :: [Int] -> [Int] -> IO ()
printMeRes [] _ = putStrLn "|"
printMeRes (x:xs) (y:ys)
    | x == 0 = do
        putStr "|    "
        printMeRes xs ys
    | x < 10 && x /= y = do
        putStr $ "|  " ++ show x ++ " "
        printMeRes xs ys
    | x < 10 = do
        putStr $ "|  "
        putChunk $ (chunk $ pack $ show x) & fore green & bold
        putStr " "
        printMeRes xs ys
    | x == y = do
        putStr $ "| "
        putChunk $ (chunk $ pack $ show x) & fore green & bold
        putStr " "
        printMeRes xs ys
    | otherwise = do
        putStr $ "| " ++ show x ++ " "
        printMeRes xs ys

printGrillRes :: Grill -> Grill -> IO ()
printGrillRes [] _ = return ()
printGrillRes (x:xs) (y:ys) = do
    putStrLn str
    printMeRes x y
    if xs == []
    then putStrLn str
    else return ()
    printGrillRes xs ys
    where str = foldl (\acc _  -> acc ++ "----+") "+" x

moveRight :: Grill -> Grill
moveRight [] = []
moveRight (x:xs)
    | (0 `elem` x) == True && last x /= 0 =
        let (Just index) = elemIndex 0 x
        in ((take index x) ++ [(x !! (index + 1))] ++ [0] ++ (drop (index + 2) x)) : moveRight xs
    | otherwise = x : moveRight xs
    
moveLeft :: Grill -> Grill
moveLeft [] = []
moveLeft (x:xs)
    | (0 `elem` x) == True && head x /= 0 =
        let (Just index) = elemIndex 0 x
        in ((take (index - 1) x) ++ [0] ++ [(x !! (index - 1))] ++ (drop (index + 1) x)) : moveLeft xs
    | otherwise = x : moveLeft xs

moveUp :: Grill -> Grill
moveUp [] = []
moveUp (x:x1:xs)
    | (0 `elem` x1) == True = 
        let (Just index) = elemIndex 0 x1
        in ((take index x) ++ [0] ++ (drop (index + 1) x)) : ((take index x1) ++ [x !! index] ++ (drop (index + 1) x1)) : moveUp xs
moveUp (x:xs) = x : moveUp xs

moveDown :: Grill -> Grill
moveDown [] = []
moveDown (x:x1:xs)
    | (0 `elem` x) == True = 
        let (Just index) = elemIndex 0 x
        in ((take index x) ++ [x1 !! index] ++ (drop (index + 1) x)) : ((take index x1) ++ [0] ++ (drop (index + 1) x1)) : moveDown xs
moveDown (x:xs) = x : moveDown xs

moveAct :: Grill -> Act -> Grill
moveAct grill act
    | act == ActLeft = moveLeft grill
    | act == ActUp = moveUp grill
    | act == ActRight = moveRight grill
    | act == ActDown = moveDown grill

toGrill :: [Int] -> Grill
toGrill tab
    | size /= (fromIntegral $ round size) || size < 3 = error "wrong grill size"
    | otherwise = newGrillList tab $ round size
    where size = sqrt $ fromIntegral $ length tab

newGrillList :: [Int] -> Int -> Grill
newGrillList [] _ = []
newGrillList tab size = take size tab : newGrillList (drop size tab) size

newGrill :: Int -> Grill
newGrill size = toGrill [0..(size^2 - 1)]