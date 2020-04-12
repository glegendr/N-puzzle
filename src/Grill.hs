module Grill
( Act(..)
, Grill
, moveRight
, moveLeft
, moveUp
, moveDown
, moveAct
, printGrill
) where

import Text.Printf
import Data.List

type Grill = [[Int]]
data Act = ActLeft | ActUp | ActRight | ActDown deriving (Eq, Ord)
instance Show Act where
    show ActLeft = "Left"
    show ActUp = "Up"
    show ActRight = "Right"
    show ActDown = "Down"

printMe :: [Int] -> IO ()
printMe [] = putStrLn "|"
printMe (x:xs)
    | x < 10 = do
        printf "|  %i " x
        printMe xs
    | otherwise = do
        printf "| %i " x
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
