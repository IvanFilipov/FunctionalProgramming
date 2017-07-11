module Exam where


import Data.List --sort

--task1

checkNotEq f g a b = all (\x -> f x /= g x) [a..b]

countIntervals f g a b = length[ (a1,b1) | a1 <- [a..b] , b1 <- [a1..b] , checkNotEq f g a1 b1]

--task3

cube = (\x -> x*x*x)

sumsOfCubes = [cube x + cube y | x <- [1..] , y <-[1..x]]

--task2

data Tree = Empty | Node Int Tree Tree deriving Show

--testing stuff
makeLeaf v = (Node v Empty Empty)
t = (Node 5 (Node 3 (Node 1 Empty (makeLeaf 2)) (makeLeaf 4)) (makeLeaf 6))
t1 = (Node 8 (Node 23 (Node 14 (makeLeaf 2) Empty) (makeLeaf 6)) (Node 11 (makeLeaf 9) (makeLeaf 27)))

minLeft (Node v Empty _) = v
minLeft (Node v lt _ ) = min  v (minLeft lt)

maxRight (Node v _ Empty) = v
maxRight (Node v _ rt) = max v (maxRight rt)

data PairedTree = EmptyP | NodeP (Int,Int) PairedTree PairedTree deriving Show

--pairTree :: Tree -> PairedTree
pairTree Empty = EmptyP
pairTree (Node v Empty Empty) = (NodeP (v,v) EmptyP EmptyP) -- not needed , but makes the things a bit faster
pairTree t@(Node v lt rt) = (NodeP (minLeft t,maxRight t) (pairTree lt) (pairTree rt))


--task4

type Shoe = (String,Int)

getName (n,_) = n
getSize (_,s) = s

lShoes = [("footballboots",18),("boots",38) , ("sandals",41),("boots",38),("sandals",43),("footballboots",12)]

getNamesList lShoes = [ getName s | s <- lShoes ] 

removeEq [] _ = []
removeEq (x:xs) curMin 
         | x == curMin = removeEq xs curMin
		 | x > curMin = (x:removeEq xs x)
		 


getRange name lShoes  = (name,listNums)
              where listNums = removeEq (sort [getSize s | s <- lShoes , getName s == name]) 0
			       

--lShoes can't be an empty list ,so using "foldl1" is safety				  
bestRange lShoes = (\(n1,x1)-> n1) 
                   (foldl1 (\a@(n1,x1) b@(n2,x2) -> if( (length x1) > (length x2)) then a else b) 
				                              [getRange n lShoes | n <- getNamesList lShoes])           

 



 