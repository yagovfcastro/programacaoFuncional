
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

trocar [x] = [x]
trocar (x:y:zs)
 | x > y = y : trocar (x:zs)
 | otherwise = x : trocar  (y:zs)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (trocar lista) (n-1)

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

--quicksort

quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs, x < s]
                   ++ [s] ++
                   quicksort [x | x <- xs, x >= s]

--11)

--bubble

trocar1 [x] = [x]
trocar1 (x:y:zs)
 | x > y = y : trocar1 (x:zs)
 | otherwise = x : trocar1  (y:zs)

bolhaOrd1 lista 0 = lista
bolhaOrd1 lista n = bolhaOrd1 (trocar1 lista) (n-1)

bolha1 [] = []
bolha1 lista = bolhaOrd1 lista (length lista)

--selection sort

selecao1:: (Ord a) => [a] -> [a]
selecao1 [] = []
selecao1 xs = [x] ++ selecao1 (remove1 x xs)
                      where x = minimo1 xs

remove1:: (Ord a) => a -> [a] -> [a]
remove1 a [] = []
remove1 a (x:xs)
 | a == x = xs
 | otherwise = x:(remove1 a xs)

minimo1:: (Ord a) => [a] ->a
minimo1 [] = undefined
minimo1 [x] = x
minimo1 (x:xs)
 | x <= (minimo1 xs) = x
 | otherwise = minimo1 xs

 --insertion sort

insercao1:: Ord a => [a] -> [a]
insercao1 = foldr insereOrd1 []

insereOrd1:: (Ord a) => a -> [a] -> [a]
insereOrd1 x [] = [x]
insereOrd1 x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y: (insereOrd1 x ys)

--quicksort

quicksort1:: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (s:xs) = quicksort1 [x | x <- xs, x < s]
                   ++ [s] ++
                   quicksort1 [x | x <- xs, x >= s]

--12)

--bubble

trocar2 [x] = [x]
trocar2 (x:y:zs)
 | x <= y = y : trocar2 (x:zs)
 | otherwise = x : trocar2  (y:zs)

bolhaOrd2 lista 0 = lista
bolhaOrd2 lista n = bolhaOrd2 (trocar2 lista) (n-1)

bolha2 [] = []
bolha2 lista = bolhaOrd2 lista (length lista)

--selection sort

selecao2:: (Ord a) => [a] -> [a]
selecao2 [] = []
selecao2 xs = [x] ++ selecao2 (remove2 x xs)
                      where x = minimo2 xs

remove2:: (Ord a) => a -> [a] -> [a]
remove2 a [] = []
remove2 a (x:xs)
 | a == x = xs
 | otherwise = x:(remove2 a xs)

minimo2:: (Ord a) => [a] ->a
minimo2 [] = undefined
minimo2 [x] = x
minimo2 (x:xs)
 | x >= (minimo2 xs) = x
 | otherwise = minimo2 xs

 --insertion sort

insercao2:: Ord a => [a] -> [a]
insercao2 = foldr insereOrd2 []

insereOrd2:: (Ord a) => a -> [a] -> [a]
insereOrd2 x [] = [x]
insereOrd2 x (y:ys)
 | x >= y = (x:y:ys)
 | otherwise = y: (insereOrd2 x ys)

--quicksort

quicksort2:: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (s:xs) = quicksort2 [x | x <- xs, x > s]
                   ++ [s] ++
                   quicksort2 [x | x <- xs, x <= s]