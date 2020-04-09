module Srcs.Heuristic
( manathan
, un
, deux
)where

import Srcs.Grill (Grill)

manathan :: Grill -> [(Int, Int)]
manathan [] = []
manathan (x:xs) = (head x, 0) : manathan xs

un :: Grill -> [(Int, Int)]
un [] = []
un (x:xs) = (head x, 1) : un xs

deux:: Grill -> [(Int, Int)]
deux [] = []
deux (x:xs) = (head x, 2) : deux xs