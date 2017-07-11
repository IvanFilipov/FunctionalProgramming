
module Try where

import Data.List


allin l1 l2 = all (\x -> x `elem` l1) l2


subset [] = [[]]
subset (x:t) =
  (subset t) ++ map (x:) (subset t) 
  
  
type Graph = [[Int]]

getSucs [] _ = []
getSucs ((hv:hs):t) v
   |v == hv = hs
   |otherwise = getSucs t v

extendPath g p = [(x:p) | x <- getSucs g (head p) , not (elem x p)]
  
--graph / front / accPaths  
allPaths _ [] paths = paths 
allPaths g (front : rest) paths =
	allPaths g (rest++(extendPath g front)) (front:paths)
		
checkPath g v t marked
    | v == t = True
	| t `elem` getSucs g v = True
	|otherwise = any (\x -> checkPath g x t (x:marked)) [x | x <- getSucs g v , not (x `elem` marked)]
       
	
	
data Tree = Empty | Node Int [Tree]


combine f g h = \x -> f x (g x (h x))

checkEq f g a b = all (\x -> f x == g x ) [a..b]

check a b uns bins = 
    0 < length[f1 | f <- bins , g <- bins , h <- uns , f1 <- uns , checkEq f1 (combine f g h) a b]
	
	
isPrime x = (x == 2) || ((x > 2) && (x `mod` 2 == 1) && [y | y <- [3,5 .. x-1] , x `mod` y == 0] == [] ) 	


	