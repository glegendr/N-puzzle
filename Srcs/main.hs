import System.Environment
import Srcs.Grill
import Srcs.Leakser
import Data.BloomFilter as Bf
import Data.BloomFilter.Hash as Bfh
import Data.BloomFilter.Easy
import Data.List as Dl
import Debug.Trace
import Data.HashMap as Hm
import Data.Heap.Binary as Bh
-- import Data.HashSet

type Child = (Int, Grill, Int)

main = do
    args <- getArgs
    if args == []
    then error "No map provided" 
    else return ()
    checkFlags args
    (grill, res, hs) <- leakser args
    printGrill grill
    putStrLn "--------------"
   -- let x = Bf.singleton (Bfh.hash32 1) 1024 grill
   --  let permuL = length $ permutations $ foldl1 (++) grill
   --      (size, numHashes) = suggestSizing permuL 0.001
   --  (Bf.singleton (Bfh.cheapHashes numHashes) size (show grill))
    -- print $ (grilltostring res) `Bf.elem` x
    -- let y = Bf.insertList [grilltostring res] x
    -- print $ (grilltostring res) `Bf.elem` y
  --  print x
    printGrill res
    putStrLn "--------------"
    printGrill $ aStar grill res hs

-- conservative, aggressive :: Double -> [B.ByteString] -> BF.Bloom B.ByteString
-- conservative = easyList
-- 
-- aggressive fpr xs
-- let (size, numHashes) = suggestSizing (length xs) fpr
--           k = 3
--       in BF.fromList (cheapHashes (numHashes - k)) (size * k) xs



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
        -- closeList = []
        closeList = Hm.singleton grill 0
        openList = Bh.singleton (0, grill, 0)
    in aStarBis closeList openList res hs

-- 125870
-- 11866

aStarBis :: Map Grill Int -> BinaryHeap Child -> Grill -> (Grill -> Int) -> Grill
aStarBis _ Leaf _ _ = []
aStarBis closeList openList res hs
    | grill == res = grill
    | otherwise = aStarBis (Hm.insert grill 0 closeList) (Bh.merge (calcHs hs $ tailR closeList xs cost $ createChildren grill) xs) res hs
    where 
        (heur, grill, cost) = Bh.head openList
        xs = Bh.tail openList
        tailR :: Map Grill Int -> BinaryHeap Child -> Int -> [Grill] -> BinaryHeap Child
        tailR _ _ _ [] = Leaf
        tailR closeList openList cost (x:xs)
            | member x closeList = tailR closeList openList cost xs
            | otherwise = Bh.insert (0, x, cost + 1) (tailR closeList openList cost xs)
        calcHs :: (Grill -> Int) -> BinaryHeap Child -> BinaryHeap Child
        calcHs _ Leaf = Leaf
        calcHs hs openList = Bh.insert (cost + (hs grill), grill, cost) (calcHs hs xs)
            where
                (heur, grill, cost) = Bh.head openList
                xs = Bh.tail openList
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
