module Srcs.Grill
( Grill
, moveRight
, moveLeft
, moveUp
, moveDown
, printGrill
) where

import Text.Printf
import Data.List

type Grill = [[Int]]

printMe :: [Int] -> IO ()
printMe [] = do  putStrLn ""
printMe (x:xs) = do
    printf "%-4i" x
    printMe xs

printGrill :: Grill -> IO ()
printGrill [] = return ()
printGrill (x:xs) = do
    printMe x
    printGrill xs

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
