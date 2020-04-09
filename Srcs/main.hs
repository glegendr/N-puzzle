import System.Environment
import Srcs.Grill
import Srcs.Leakser
-- import Data.BloomFilter as Bf
-- import Data.BloomFilter.Hash as Bfh
import Data.List
import Debug.Trace
-- import Data.HashSet

type Child = (Grill, Int, Int)

main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    (grill, res, hs) <- leakser args
    printGrill grill
    putStrLn "--------------"
 --   let x = Bf.fromList (Bfh.cheapHashes 1) 666 [grilltostring grill]
    -- print $ (grilltostring res) `Bf.elem` x
    -- let y = Bf.insertList [grilltostring res] x
    -- print $ (grilltostring res) `Bf.elem` y
  --  print x
    printGrill res
    putStrLn "--------------"
    printGrill $ aStar grill res hs



{--
    openList    -> [(Grill, cout, heuristique)]
                    -> Grill              == grill,
                    -> cout::Int          == Nb noeuds précédent
                    -> heuristique::Int   == fct heuristique + cout
    closeList   -> BloomFilter String
                    -> Bf == noeuds déjà croisés, hashés
--}
aStar :: Grill -> Grill -> (Grill -> Int) -> Grill
aStar grill res hs =
    let --closeList = Bf.fromList (Bfh.cheapHashes 1) 666 [grillToString grill]
        closeList = []
        openList = [(grill, 0, 0)]
    in aStarBis closeList openList res hs

-- 125870
-- 11866

aStarBis :: [Grill]-> [Child] -> Grill -> (Grill -> Int) -> Grill
aStarBis _ [] _ _ = []
aStarBis closeList ((grill, cost, heur):xs) res hs
    | grill == res = grill
    | otherwise = aStarBis (grill : closeList) (sortOn (\(_, _, h) -> h) ((calcHs hs $ tailR closeList xs cost $ createChildren grill) ++ xs)) res hs
    where 
        tailR :: [Grill]-> [Child] -> Int -> [Grill] -> [Child]
        tailR _ _ _ [] = []
        tailR closeList openList cost (x:xs)
            | x `elem` closeList || any (\(x, y, _) -> x == grill && y <= cost) openList = tailR closeList openList cost xs
            | otherwise = (x, cost + 1, 0) : tailR closeList openList cost xs
        calcHs :: (Grill -> Int) -> [Child] -> [Child]
        calcHs _ [] = []
        calcHs hs ((grill, cost, heur):xs) = (grill, cost, cost + (hs grill)) : calcHs hs xs
        createChildren :: Grill -> [Grill]
        createChildren grill = [moveRight grill] ++ [moveLeft grill] ++ [moveUp grill] ++ [moveDown grill]

 
 {--
   Fonction cheminPlusCourt(g:Graphe, objectif:Nœud, depart:Nœud)
       closedList = File()
       openList = FilePrioritaire(comparateur=compare2Noeuds)
       openList.ajouter(depart)
       tant que openList n'est pas vide
           u = openList.depiler()                       -> u = openList.pop() -> cout le plus faible
           si u.x == objectif.x et u.y == objectif.y    -> pas arriver objectif == comparer avec res
               reconstituerChemin(u)
               terminer le programme
           pour chaque voisin v de u dans g            ->créer 4 fils -> bas+haut ...
               si non(v existe dans closedList ou si v existe dans openList avec un cout inférieur) -> verifier 
                    v.cout = u.cout +1 
                    v.heuristique = v.cout + distance([v.x, v.y], [objectif.x, objectif.y])
                    openList.ajouter(v)                -> ajouter fils
           closedList.ajouter(u)                        -> ajouter u
       terminer le programme (avec erreur)
--}
