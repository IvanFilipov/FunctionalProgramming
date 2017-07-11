module Preparation where

import Data.List

data Tree = Empty | Node Int Tree Tree deriving Show

makeLeaf val = Node val Empty Empty


bloom Empty = Empty
bloom (Node val Empty Empty) = (Node val (makeLeaf val) (makeLeaf val))
bloom (Node r lt rt) = (Node r (bloom lt) (bloom rt))

height Empty = 0
height (Node _ rt lt) = 1 + max (height rt) (height lt)

evenCnt Empty = 0
evenCnt (Node val rt lt) 
       | val `mod` 2 == 0 = 1 + evenCnt rt + evenCnt lt
	   |otherwise =  evenCnt rt + evenCnt lt
	   
	   
	   
data Mtree = MEmpty | MNode Int [Mtree] deriving Show

makeMLeaf val = MNode val []

maxiMTree (MNode val []) = val
maxiMTree (MNode val tl) = foldl (\x y-> if(x > y) then x else y ) val (map maxiMTree tl) 


type Graph = [[Int]]

getSucs [] _ =  []
getSucs ((x:l):g) s 
    | x == s = l
	|otherwise  = getSucs g s

extendPath g p =
   [(x:p) | x <- getSucs g (head p) , not (elem x p) ]
	
getVertexes g = [head s | s <- g ] 
		
maxLen lList = foldl (\ x y -> if (length x > length y) then x else y) [] [l | l <- lList]	
	
gpaths _ [] paths = paths
gpaths g (front : rest) paths = 
    gpaths g (rest++(extendPath g front)) (front:paths)

allPaths g = foldl (++) [] [l | l <- [gpaths g [[x]] [] | x <- getVertexes g]]
	
maxCycle g v =  reverse (maxLen [v:p | p <- allPaths g  ,v == (last p) , v `elem` (getSucs g (head p))])


-------

subset [] = [[]]
subset (x:t) = 
    (subset t) ++ map (x:) (subset t)

	

type Play = (String ,Int ,Int)

getName (n,_,_) = n
getStart (_,s,_ ) = s
getDur (_,_,d) = d


canBePlayed lPlays s =
   length lPlays == length [x | x <- lPlays , ( s - getStart x ) >= 0 , getStart x + getDur x `div` 60 >= s + 1]
 
allPl lPlays = [p | p <- subset lPlays ,x <- [0..23] ,canBePlayed p x ]


allin l1 l2 = 
    all (\x -> elem x l2) l1
	
columsMatrix matrix = length [l | l <- transpose matrix , any (allin l) matrix]

findMatrix matrix = (length [l | l <- transpose matrix , all (allin l) matrix]) > 0


sm matrix = foldl (+) 0 [foldl (*) 1 l | l <- matrix]




	
	
	
	
	