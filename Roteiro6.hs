
--1)

paridade:: [Int] -> [Bool]
paridade l = map (even) l

--2)

prefixos:: [String] -> [String]
prefixos l = map (take 3) l

--3)

saudacao:: [String] -> [String]
saudacao l = map ("Oi " ++ ) l

--4)

filtrar p xs = [ x | x <- xs, p x]

--5)

pares:: [Int] -> [Int]
pares l = filter (even) l

--6)

solucoes:: [Int] -> [Int]
solucoes l = filter (\x -> 5*x + 6 < x * x) l

--7)

maior:: [Int] -> Int
maior l = foldr1 (max) l

--8)

menor_min10:: [Int] -> Int
menor_min10 l = foldr (min) 10 l

--9)

junta_silabas_plural:: [String] -> String
junta_silabas_plural l = foldr (++) "s" l

--10)

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

--bubble

troca [x] = [x]
trocar (x:y:zs)
 | x > y = y : troca (x:zs)
 | otherwise = x : trocar  (y:zs)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

bolha [] = []
bolha lista = bolhaOrd lista (length lista)

--selection sort

selecao:: (Ord a) => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
                      where x = minimo xs

remove:: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
 | a == x = xs
 | otherwise = x:(remove a xs)

minimo:: (Ord a) => [a] ->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
 | x <= (minimo xs) = x
 | otherwise = minimo xs

 --insertion sort

insercao:: Ord a => [a] -> [a]
insercao = foldr insereOrd []

insereOrd:: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y: (insereOrd x ys)

quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs, x < s]
                   ++ [s] ++
                   quicksort [x | x <- xs, x >= s]
 