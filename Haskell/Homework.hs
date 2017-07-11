module Homework where 

--task1
hailstone x 
	|x == 1 = [1]
	|x `mod` 2 == 0 = x : hailstone (x `div` 2)
    |otherwise =  x : hailstone (x*3 + 1)
	
	
--task2	   
isPrime x = 
           (x == 2) ||
           (x > 2 && (mod x 2 == 1) && ([d | d <- [3,5..x-1] , x `mod` d == 0] == []))
		   

sums =
  [ a + 2 * b | a <- [primes | primes <- [2..100] , isPrime primes] 
  , b <- [squares*squares | squares <- [1..10]] ]

numsCnt = length [x | x <- [11,13..100] , not (x `elem` y)] 	
    where y = sums


--task3

times x y 
    | x `mod` y /= 0 = 0
    | otherwise = 1 + times (x `div` y) y
	
divs x y
    | y > x || x == 1 = []
    | x `mod` y == 0 = let t = times x y
                       in ((y,t) : (divs (x `div` (y^t)) (y+1)))
    | otherwise = divs x (y+1)

divisors x 
    | x < 2 = []
    | otherwise = divs x 2
     
         



--task4
intercalate' _ [] = ""

intercalate' s (hs:ts) 
             | ts == [] = hs
             |otherwise = (hs++s ++ intercalate' s ts)
   
   
--task5

sumCouples [] = []
sumCouples (hs1:(hs2:ts)) =
			(hs1 + hs2) : sumCouples ts


fenwick [] = [[]]
fenwick (hs:ts)  
        | ts == [] = [[hs]]
		|otherwise = 
          let curLine = sumCouples (hs:ts)
	      in  ((fenwick curLine) ++ [(hs:ts)]) 
    
 
   
   