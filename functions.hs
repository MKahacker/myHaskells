--------------------------------------------Helper functions-------------------------------------------------------
--Replicates the raise function (^) created because of dist functions below
raise:: (Num b, Num t, Enum t) => b -> t -> b
raise x y = foldr (*) 1 [x | i <- [1..y]] 

-----------------------------------------------End section---------------------------------------------------------


------------------------------------Functions that work on lists---------------------------------------------------
--Function that applies a two parameter function on two same size list and returns a list
funTwoLsts :: (a -> a -> b) -> [a] -> [a] -> [b]
funTwoLsts f [] [] = []
funTwoLsts f (x:xs) (y:ys) = (f x y : funTwoLsts f xs ys)

--Function that makes lists using a function with one parameter and a loop that feeds (0 to n) arguements
funListMaker :: (Enum a, Eq a, Num a) => (a -> b) -> a -> [b]
funListMaker f 0 = []
funListMaker f n = [f x | x <- [0..n]]

-----------------------------------------------End section---------------------------------------------------------


------------------------------------------Functions for trig -----------------------------------------------------------
--Distance between two points on a graph
distance:: (Floating a) => (a,a) -> (a,a) -> a
distance (x,y) (x',y') =  sqrt(((x' - x)^2) + ((y' - y)^2))

--Centripial accelaration
centripial:: (Fractional a) => a -> a -> a
centripial v r = (v^2)/r

--Magnitude of a vector
magnitude:: (Floating a) => a -> a -> a
magnitude x y = sqrt((x^2) + (y^2))

-----------------------------------------------End section---------------------------------------------------------


---------------------------------------------Probability-------------------------------------------------------- 
--Factorial function
fact :: (Eq a, Num a) => a -> a
fact 0 = 1
fact n = n*(fact (n-1))

--Gamma function
gamma:: (Eq a, Num a, Ord a) => a -> a
gamma x  
      |(x <= 0) = undefined
      |(x == 1) = 1 
      |(x > 1) = (x-1)*(gamma (x-1))

--Combination function relies on factorial
choose:: (Fractional a, Eq a) => a -> a -> a
choose x n = (fact n)/((fact x)*(fact (n-x)))

--Permutation function relies on factorial
permutation:: (Fractional a, Eq a) => a -> a -> a
permutation x n = (fact n)/(fact x)

--Hypergeomerty distribution relies on choose
hypergeometry:: (Fractional a, Eq a) => a -> a -> a -> a -> a
hypergeometry x k n s = (choose x k)*(choose (n-x) (s-k))/(choose n s)

--Binomial distribution relies on choose and raise
binomial::  (Fractional a, Eq a, Enum a) => a -> a -> a -> a
binomial x n p = (choose x n)*(raise p x)*(raise (1-p) (n-x))

--Possion distribution relies on choose and raise
possion:: (Fractional a, Floating a, Eq a, Enum a) => a -> a -> a
possion x l = (raise l x)/((raise (exp 1) l)*(fact x))

-----------------------------------------------End section---------------------------------------------------------