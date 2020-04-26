import System.Environment
import Grill
import Leakser
import Heuristic
import Algo
import Parsing(checkGrill)
import Benchmark
import Actions
import System.Console.ANSI
import Control.Concurrent
import System.Exit
import Data.Char

isThatReal :: Grill -> Grill -> [Act] -> Bool
isThatReal grill res act = doSteps grill act == res

doSteps :: Grill -> [Act] -> Grill
doSteps grill [] = grill
doSteps grill (x:xs) = doSteps (moveAct grill x) xs

interactive_help :: IO ()
interactive_help = do
    putStrLn "Give us an action as:\n<Left>/<L> <Up>/<U> <Right>/<R> <Down>/<D>\nOr a command like:\n<quit> <help>"
    return ()

getActFromUser :: IO Act
getActFromUser = do
    putStrLn "Give your move: "  
    l <- getLine
    matchMove l
    where
        matchMove :: String -> IO Act
        matchMove x
            | str == "quit" = exitWith ExitSuccess
            | str == "help" = do
                interactive_help
                getActFromUser
            | otherwise = return $ fromString str
            where str = map toLower x  

inverse :: Grill -> Grill -> [Act] -> String -> Int -> IO ()
inverse grill res act v strokes
    | strokes == 0 && act /= [] =  do
        printVisu grill res v act
        let end = doSteps grill act 
        if (end == res)
        then putStrLn "The List is solving the puzzle"
        else (
            do
                putStrLn "The List is not solving the puzzle\nExpected:"
                printGrill res
                putStrLn "Got:"
                printGrillRes2 end res)
        exitWith ExitSuccess
    | strokes == 0 = do
        putStrLn ""
        printGrill res
        printGrillRes grill res
        inverse grill res [] v 1
    | grill == res = do
        putStrLn $ "Bravo !\nNumber of moves: " ++ show (strokes - 1) ++ "\nMoves: " ++ (show $ reverse act) ++ "\nReduce Moves: " ++  (map toSingleLetter $ reverse act)
        exitWith ExitSuccess
    | otherwise = do
        newAct <- getActFromUser
        printGrill res
        let newGrill = moveAct grill newAct
        printGrillRes newGrill res
        putStrLn $ "Strokes: " ++ show strokes
        inverse newGrill res (newAct:act) v (strokes + 1)

main = do
    args <- getArgs
    if args == []
    then helper
    else return ()
    checkFlags args
    (grill, res, hf, af, visu, (b, m)) <- leakser args
    if (b == True)
    then inverse grill res m visu 0
    else return () 
    checkGrill grill res
    let (moves, acts, time, mem) = aStarBench grill res (algorithmFunction af hf res)
    let newActs = Actions.insert (foldl (\tree act -> Actions.insert tree act False) Actions.new acts) moves True
    printVisu grill res visu $ reverse moves
    putStrLn $ "Time Complexity: " ++ (show time)
    putStrLn $ "Memory Complexity: " ++ (show mem)
    putStrLn $ "Number of moves: " ++ (show (length moves))
    putStrLn $ "Moves: " ++ (show $ reverse moves)
    putStrLn $ "Reduce Moves: " ++ (map toSingleLetter $ reverse moves)

printVisu :: Grill -> Grill -> String -> [Act] -> IO ()
printVisu grill res visu act
    | visu == "0" || visu == "empty" = return ()
    | visu == "1" || visu == "partial" = do
        printGrill grill
        putStrLn ""
        printGrill res
        putStrLn ""
    | visu == "2" || visu == "all" = putMoves (length act) False grill act res
    | visu == "3" || visu == "animated" = putMoves (length act) True grill act res
    | otherwise = return ()

putMoves :: Int -> Bool -> Grill -> [Act] -> Grill -> IO ()
putMoves _ True grill [] _ = putStrLn $ replicate (length grill * 2 + 1) '\n'
putMoves _ _ _ [] _ = return ()
putMoves nbMoves animated grill (x:xs) res
    | animated == True = do
        printGrillRes newGrill res
        putStrLn ""
        putStrLn $ "Move: " ++ show (nbMoves - length xs) ++ "/" ++ show nbMoves ++ "    "
        putStrLn $ "Current Move: " ++ show x ++ "    "
        cursorUpLine $ (length grill) * 2 + 4
        threadDelay 200000
        putMoves nbMoves animated newGrill xs res
    | otherwise =  do
        printGrill newGrill
        putStrLn ""
        putMoves nbMoves animated newGrill xs res
    where newGrill = moveAct grill x
