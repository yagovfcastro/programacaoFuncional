--1)

--a 

-- (||):: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False 

--b

-- (||):: Bool -> Bool -> Bool
-- a || b 
-- | a == True || B == True = True
-- | otherwise = False

--2)

distancia:: (Float,Float) -> (Float,Float) -> Float
distancia (a,b) (c,d) =  sqrt ((c - a)^2 + (d - b)^2)

--3)

-- 1:[2,3,4] == [1,2,3,4]
-- 'a':['b','c','d'] == "abcd"
-- head [1,2,3] == 1
-- tail [1,2,3] == [2,3]
-- [1,5,2,3]!!1 == 5
-- [1,5,2,3]!!3 == 3
-- elem 2 [1,5,2,3] == True
-- take 2 [1,5,2,3,7] == [1,5]
-- drop 2 [1,5,2,3,7] == [2,3,7]
-- [1,2] ++ [3,4] == [1,2,3,4]
-- [1..10] == [1,2,3,4,5,6,7,8,9,10]
-- [7,6..3] == [7,6,5,4,3]
-- ['b'..'g'] == "bcdefg"
-- take 5 [1,3..] == [1,3,5,7,9]
-- sum [1..10] == 55
-- maximum [1,5,2,3,7] == 7
-- minimum [1,5,2,3,7] == 1

--4)

fatorial:: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial(n-1) 

fatorial2:: Int -> Int
fatorial2 n
 | n == 0 = 1
 | otherwise = n * fatorial2(n-1)

 --5)

fibonacc1:: Int -> Int
fibonacc1 0 = 0
fibonacc1 1 = 1
fibonacc1 n = fibonacc1(n-2) + fibonacc1(n-1)

fibonacci:: Int -> Int
fibonacci n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fibonacci(n-2) + fibonacci(n-1)

 --6)

n_tri:: Int -> Int
n_tri 1 = 1
n_tri 2 = 3
n_tri n = n + n_tri(n - 1)

--7)

passo:: (Int,Int) -> Int
passo (x,y) = fibonacc1 x

fibo2:: Int -> Int
fibo2 0 = 1
fibo2 1 = 1
fibo2 n = passo(n, n+1)


--8)

potencia2:: Int -> Int
potencia2 0 = 1
potencia2 n = 2 * potencia2 (n - 1)

--9)

--a

prodIntervalo:: Int -> Int -> Int
prodIntervalo m n
 | m == n = m
 | otherwise = m * (prodIntervalo (m+1) n)

--b

fat3:: Int -> Int
fat3 n =  prodIntervalo 1 n

--11)

resto_div:: Int -> Int -> Int
resto_div m n
 | n > m = m
 | otherwise = resto_div (m-n) n

div_inteira:: Int -> Int -> Int
div_inteira m n
 | m < n = 0
 | m == n = 1
 | otherwise = 1 + div_inteira (m-n) n

--12)

mdc:: (Int, Int) -> Int
mdc (m,n)
 | n == 0 = m
 | otherwise = mdc (n, (mod m n))

mdc2:: (Int, Int) -> Int
mdc2 (m,0) = m
mdc2 (m,n) = mdc2 (n, (mod m n))

--13)

binomial:: (Int, Int) -> Int
binomial (n,k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomial(n-1, k) + binomial(n-1,k-1)

binomial2:: (Int, Int) -> Int
binomial2 (n,0) = 1
binomial2 (n,k) = if (k == n) 
  then 1 
  else binomial2 (n-1,k) + binomial2 (n-1,k-1)

--14)

--a [5,4,3,2,1] == [5,4..1]
--b [a,c,e] == ['a','c'..'e']
--c [1,4,7,10,13,16] == [1,4..16]
--d [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)] == zip [1,(-2)..(-11)] [1,5..17]

--15)

--a

constroiLista:: Int -> Int -> [Int]
constroiLista a b
 | a > b = []
 | a == b = [a]
 | otherwise = [a..b]

 --b

listaPares:: Int -> Int -> [Int]
listaPares a b
 | a > b = []
 | a == b && even b = [b]
 | otherwise = if even a 
   then a:listaPares (a+1) b 
   else listaPares (a+1) b
 



