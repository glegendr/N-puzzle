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
-- import Debug.Trace

isThatReal :: Grill -> Grill -> [Act] -> Bool
isThatReal grill res [] = grill == res
isThatReal grill res (x:xs) = isThatReal (moveAct grill x) res xs

main = do
    args <- getArgs
    if args == []
    then helper
    else return ()
    checkFlags args
    (grill, res, hf, af, visu) <- leakser args
    checkGrill grill res
    let (moves, acts, time, mem) = aStarBench grill res (algorithmFunction af hf res)
    let newActs = Actions.insert (foldl (\tree act -> Actions.insert tree act False) Actions.new acts) moves True
    printVisu grill res visu $ reverse moves
    -- writeFile "docs/oui.json" (show newActs)
    putStrLn $ "Time Complexity: " ++ (show time)
    putStrLn $ "Memory Complexity: " ++ (show mem)
    putStrLn $ "Number of moves: " ++ (show (length moves))
    putStrLn $ "Moves: " ++ (show $ reverse moves)

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
