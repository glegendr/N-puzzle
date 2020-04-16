module Chart
(chartMe
, generateMax
, minmax
, barChart
, spiderChart
, spiderChartEasy) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)  
import Data.List
import Data.Time
import Control.DeepSeq

data Bench = AlgoName | HeuristicName | MapName | TimeComplexity | MemoryComplexity | NumberOfMoves | Time deriving (Eq)
instance Show Bench where
    show AlgoName = "Name"
    show HeuristicName = "Heuristic Name"
    show MapName = "Map Name"
    show TimeComplexity = "Time Complexity"
    show MemoryComplexity = "Memory Complexity"
    show NumberOfMoves = "Number Of Moves"
    show Time = "Time"

minmax :: (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> (Double, Double, Double, Double)
minmax (maxT, maxM, maxMC, maxTC) (t, m, mC, tC) =
  (1000 * t / maxT, 1000 * m / maxM, 1000 * mC / maxMC, 1000 * tC / maxTC)


generateMax :: [(Double, Double, Double, Double)] -> (Double, Double, Double, Double)
generateMax list =
  let
    maxtTime = maximum $ map (\(_, _, _, time) -> time) list
    maxMoves = maximum $ map (\(_, _, time, _) -> time) list
    maxMemoryC = maximum $ map (\(_, time, _, _) -> time) list
    maxTimeC = maximum $ map (\(time, _, _, _) -> time) list
  in (maxTimeC, maxMemoryC, maxMoves, maxtTime)

repeatLineNTimes (n:[]) (x:[]) (y:[]) (z:[]) (a:[]) = do plot (line n [[x, y, z, a, x]])
repeatLineNTimes (n:ns) (x:xs) (y:ys) (z:zs) (a:as) = do
  plot (line n [[x, y, z, a, x]])
  repeatLineNTimes ns xs ys zs as

repeatPointsNTimes (n:[]) (x:[]) (y:[]) (z:[]) (a:[]) = do plot (points n [x, y, z, a, x])
repeatPointsNTimes (n:ns) (x:xs) (y:ys) (z:zs) (a:as) = do
  plot (points n [x, y, z, a, x])
  repeatPointsNTimes ns xs ys zs as

spiderChart :: String -> [((String, String, String), Double, Double, Double, Double)] -> IO ()
spiderChart path lst =
  toFile def path $ do
    let
      nameList = map (\((algo, heur, _), _, _, _, _) -> algo ++ " " ++ heur) lst
      ((_, _, mapName), _, _, _, _) = head lst 
      max = generateMax $ map (\((_, _, _), tC, mC, moves, time) -> (tC, mC, moves, time)) lst
      myData = map (minmax max) $ map (\((_, _, _), tC, mC, moves, time) -> (tC, mC, moves, time)) lst
      tCList = map (\(tC, _, _, _) -> (0.0, tC)) myData
      mCList = map (\(_, mC, _, _) -> (-mC, 0.0)) myData
      movesList = map (\(_, _, moves, _) -> (0.0, -moves)) myData
      timeList = map (\(_, _, _, time) -> (time, 0.0)) myData
    layout_title .= mapName
    repeatLineNTimes nameList tCList mCList movesList timeList
    repeatPointsNTimes nameList tCList mCList movesList timeList
    plot (line "" [[(0,0)]])
    return ()

spiderChartEasy :: String -> [(String, (Double, Double, Double, Double))] -> IO ()
spiderChartEasy name lst =
  toFile def ("chart/" ++ name ++ ".svg") $ do
  --  let newList = map (\(_, tuple) -> tuple) lst
    let newData = map (\(_, tuple) -> tuple) lst
    let nameList = map (\(name, _) -> name) lst
    let maxed = map (minmax (generateMax newData)) newData
    let tCList = map (\(tC, _, _, _) -> (0.0, -tC)) maxed
    let mCList = map (\(_, mC, _, _) -> (mC, 0.0)) maxed
    let movesList = map (\(_, _, moves, _) -> (0.0, moves)) maxed
    let timeList = map (\(_, _, _, time) -> (-time, 0.0)) maxed
    layout_title .= name
  -- layoutlr_grid_last .~ False
    repeatLineNTimes nameList tCList mCList movesList timeList
    repeatPointsNTimes nameList tCList mCList movesList timeList
    return ()

    

    

titles :: [(String, [(String, String, Double, Double, Double, Double)])] -> [String]
titles [] = []
titles ((_, lst):xs) = map (\(hf, _, _, _, _, _) -> hf) lst


createNewData :: String -> [((String, String, String), Double, Double, Double, Double)] -> [(String, [(String, String, Double, Double, Double, Double)])]
createNewData _ [] = []
createNewData str (((name, hf, map), t, m, c, t2):xs)
  | name == str = createNewData name xs
  | otherwise = (name, oui name $ ((name, hf, map), t, m, c, t2):xs) : createNewData name xs
  where
    oui :: String -> [((String, String, String), Double, Double, Double, Double)] -> [(String, String, Double, Double, Double, Double)]
    oui _ [] = []
    oui str (((name, hf, map), t, m, c, t2):xs)
      | name == str = (hf, map, t, m, c, t2) : oui name xs
      | otherwise = []

getValues :: Bench -> [(String, [(String, String, Double, Double, Double, Double)])] -> [(String, [Double])]
getValues bench [] = []
getValues bench ((name, lst): xs)
  | bench == Time = (name, map (\(_, _, _, _, _, time) -> time) lst) : getValues bench xs
  | bench == NumberOfMoves = (name, map (\(_, _, _, _, nbMoves, _) -> nbMoves) lst) : getValues bench xs
  | bench == MemoryComplexity = (name, map (\(_, _, _, mC, _, _) -> mC) lst) : getValues bench xs
  | bench == TimeComplexity = (name, map (\(_, _, tC, _, _, _) -> tC) lst) : getValues bench xs
  | otherwise = (name, map (\(_, _, _, _, moves, _) -> moves) lst) : getValues bench xs

barChart :: String -> Bench -> [((String, String, String), Double, Double, Double, Double)] -> IO ()
barChart path bench lst = 
  toFile def path $ do
  let ((_, _, mapName), _, _, _, _) = head lst 
      myData = createNewData "" $ sortOn (\((name, _, _), _, _, _, _) -> name) $ lst
      values = getValues bench $ myData
  layout_title .= (show bench) ++ " " ++ mapName
  layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
  plot $ fmap plotBars $ bars (titles myData) (addIndexes (map snd values))
  return ()

chartMe listMe listMe2 = do
  spiderChart "chart/spider4x4.svg" listMe
  spiderChart "chart/spider3x3.svg" listMe2 --(filter (\((algo, _, _), _, _, _, _) -> algo == "aStar") listMe)
  spiderChart "chart/spiderAStar.svg" (filter (\((algo, _, _), _, _, _, _) -> algo == "aStar") listMe2)
  barChart "chart/time4x4.svg" Time listMe
  barChart "chart/timeComplexity4x4.svg" TimeComplexity listMe
  barChart "chart/memoryComplexity4x4.svg" MemoryComplexity listMe
  barChart "chart/Moves4x4.svg" NumberOfMoves listMe
  barChart "chart/time3x3.svg" Time listMe2