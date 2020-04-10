module Srcs.Heuristic
( Heuristic 
, manathan
, un
, dijkstra
) where

import Srcs.Grill (Grill)
type Heuristic = (Grill -> Grill -> Int)

manathan :: Heuristic
manathan [] _ = 0
manathan (x:xs) _ = 0

un :: Heuristic
un [] _ = 0
un (x:xs) _ = 0

dijkstra:: Heuristic
dijkstra _ _ = 0