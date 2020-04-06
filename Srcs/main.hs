import System.Environment
import Srcs.Grill
import Srcs.Leakser

main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    ((size, grill), (size2, res), hs) <- leakser args
    if size /= size2
    then error "Not same size between given map and result map"
    else return ()
    printGrill grill
    print $ hs grill
    putStrLn "--------------"
    printGrill res
