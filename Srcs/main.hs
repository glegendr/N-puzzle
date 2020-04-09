import System.Environment
import Srcs.Grill
import Srcs.Leakser

main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    (grill, res, hs) <- leakser args
    printGrill grill
    print $ hs grill
    putStrLn "--------------"
    printGrill res
