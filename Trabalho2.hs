
--Listas 
l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

--2)

--bubble original

trocar [x] = [x]
trocar (x:y:zs)
 | x > y = y : trocar (x:zs)
 | otherwise = x : trocar  (y:zs)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (trocar lista) (n-1)

bolha [] = []
bolha lista = bolhaOrd lista (length lista)

--variação 1

trocar1 [x] = [x]
trocar1 (x:y:zs)
 | x > y = y : trocar1 (x:zs)
 | otherwise = x : trocar1  (y:zs)

bolhaOrd1 lista 0 = lista
bolhaOrd1 lista n 
 | (trocar lista) == lista = lista
 | otherwise = bolhaOrd1 (trocar1 lista) (n-1)

bolha1 [] = []
bolha1 lista = bolhaOrd1 lista (length lista)

--variação 2

trocar2 [x] = [x]
trocar2 (x:y:zs)
 | x > y = y : trocar2 (x:zs)
 | otherwise = x : trocar2  (y:zs)

bolhaOrd2 lista 0 = lista
bolhaOrd2 [] _ = []
bolhaOrd2 lista n = bolhaOrd2 (take (n-1) aux) (n-1) ++  drop (n-1) aux
                      where aux = trocar lista 
                                    
bolha2 [] = []
bolha2 lista = bolhaOrd2 lista (length lista)

--variação 3

trocar3 [x] = [x]
trocar3 (x:y:zs)
 | x > y = y : trocar3 (x:zs)
 | otherwise = x : trocar3  (y:zs)

bolhaOrd3 lista 0 = lista
bolhaOrd3 lista n 
  | (trocar lista) == lista = lista
  | otherwise = bolhaOrd3 (take (n-1) aux) (n-1) ++  drop (n-1) aux
 where 
     aux = trocar lista

bolha3 [] = []
bolha3 lista = bolhaOrd3 lista (length lista)

--Contador

--variação1

--Avaliações comparativas

--Número de comparações)

-- Podemos perceber a diferença de desempenho nos algoritmos conforme o número de comparações que cada um executa
-- Abaixo eles estão listados, com suas comparações



--Tempo de execução)

-- Podemos, claramente, perceber uma diferença no tempo de execução de cada um dos algoritmos, principalmente quando
-- utilizamos uma lista grande, como, por exemplo, a l6.
-- Organizando-os por tempo de execução (em ordem ascendente):
--                                                              variação1
--                                                              variação3
--                                                              variação2
--                                                              original

--4)

--quicksort original

quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs, x < s]
                   ++ [s] ++
                   quicksort [x | x <- xs, x >= s]

--variação 1

divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide j (x:xs)
  | x < j     = ((x:menor), maior)
  | otherwise = (menor, (x:maior))
    where (menor, maior) = divide j xs

quicksort1:: (Ord a) =>[a] -> [a]
quicksort1 [] = []
quicksort1 (s:xs) = quicksort1 (fst m1) ++ [s] ++ quicksort1 (snd m1)
                     where m1 = divide s xs 

--variação 2


        



