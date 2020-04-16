import System.Environment
import Grill
import Leakser
import Heuristic
import Algo
import Parsing(checkGrill)
import Benchmark
import Actions
-- import Debug.Trace

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
    writeFile "chart/oui.json" (show newActs)
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
    | visu == "2" || visu == "all" = putMoves grill act
    | otherwise = return ()

putMoves :: Grill -> [Act] -> IO ()
putMoves _ [] = return ()
putMoves grill (x:xs) = do
    printGrill newGrill
    putStrLn ""
    putMoves newGrill xs
    where newGrill = moveAct grill x
