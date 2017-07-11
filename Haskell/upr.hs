module UPR where

import Data.List


sm m =
    foldl (+) 0 [foldl (*) 1 l | l <- m] 

type Student=(String,Integer,Double)

-- getInfo ::[Student] ->[Student]

getInfo [] = []
getInfo (elem@(name,fn,_):xs) =
     if fn > 40000 && fn < 50000
	    then (elem : getInfo xs)
	 else
	     getInfo xs

		 
isInformatics (_,fn,_) = 
     ( fn > 40000 ) && ( fn < 50000 )


type Item = (String,Integer)

--info :: [Item] -> (string,integer,string) // nay -skoro,kolko sa iztekli ,nay -skoro weche iztekyl


getSOk list = 
    fst (foldl (\(n1,e1) (n2,e2) -> if (e1 < e2) then (n1,e1) else (n2,e2)) ("",1000000)  [(n,e) | (n,e) <- list , (e >= 0)])

cntBadList list = 
  length ([(x,v) | (x,v) <- list , (v < 0)])	
	
info [] = ("",0)
info list = (getSOk list, cntBadList list)


--additional with PESHE

--task1 
allin l1 l2 = 
    all (\x -> elem x l2) l1
	
columsMatrix = length [l | l <- transpose matrix , any (allin l) matrix]

--task2

combine f g h = \x -> h (f x) (g x)

checkEq f g a b = all (\x-> f x == g x) [a..b]

check uns bins a b =  any id [checkEq f1 (combine f h g) a b | f <- uns , g <- uns , h <- bins , f1 <- uns ]

--any (\(f,g,h,f1) -> checkEq f1 )

--task3

type Flower = (String , Int , Int )

getName (name , _ ,_) = name

garden :: [Flower] -> ((Int, Int) , [String])
garden flowers = transform foldl1 (\x y -> if length x > length y then x else y) [g | g <- flowers , checkGarden g]

--sywmestimo ?
checkGarden [] = True
checkGarden garden = 
	minMaxes >= maxMins 
	where minMaxes = foldr1 min [getMax x | x <- garden]
          maxMins = foldr1 max [getMin x | x <- garden]	

		  
subset [] = []
subset (x:t) = 
    (subset t) ++ map (x:) (subset t)
	
transform garden = ((min1,max1),names) 
    where 
	      min1 = foldl1 max [getMin x | x <- garden]
		  max1 = foldl1 mix [getMan x | x <- garden]
		  names = [getName x | x <- garden]
		  
		  
--bonus task

dist (x1,x2) (y1, y2) =  1 --:D

maxDist pnts dist = 
   foldl1 (\x y -> if(dist x > dist y) then x else y) [(x,y) | x <- pnts , y <- pnts , x/= y]
   

type StudentF = (String , Int , [Int]) 
   
getName (name , _ , _) = name
getFn (_,fn,_) = fn
getScore (_,_,gr) = sum gr / length gr
isExelStudent stud = getScore stud >= 5.50
getProfile stud = 
    | (getFn stud) `div` 10000 == 4 = "Informatics"
	| (getFn stud) `div` 10000 == 6 = "KN" 
	| otherwise = "NA"
	
getInfScore students = 
    calcAvg [getScore s | s <- students , getProfile s == "Informatics"]
	
calcAvg :: Fractional a => [a] -> a	
calcAvg [] = 0
calcAvg list = sum list / fromIntegral  (length list) 	
 

--task4

type Graph = [(a,[a])]

getSucs [] _ = []

getSucs ((x,s) : g) a 
    | x == a = s
    | otherwise = getSucs g a

extendPath g p = 
   [(x:p) <- getSucs (head p) , not (elem x p) ]
   
--grapth ,front, accPaths
getAllPaths _ [] paths = paths
getAllPaths g (path : rest) paths = 
    getAllPaths g (rest++extendPath g path) (path:paths)
	
maxPath g x = reverse ( foldl1 (\x y -> if( length x > length y) then x else y) getAllPaths g [[x]] [] )



    
	



















