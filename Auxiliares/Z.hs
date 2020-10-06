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

headAcc (l, acc) x = (x:l, acc+1)

trocarc ([x],acc) = ([x],acc)
trocarc ((x:y:zs), acc)
 | x > y = headAcc (trocarc ((x:zs), acc)) y
 | otherwise = headAcc (trocarc  ((y:zs), acc)) x

bolhaOrdc (lista,acc) 0 = (lista,acc)
bolhaOrdc (lista,acc) n = bolhaOrdc (trocarc (lista,acc)) (n-1)

bolhac [] = ([],0)
bolhac lista = bolhaOrdc (lista,0) (length lista)

-- variação 1 com contador

trocar1c ([x],acc) = ([x],acc)
trocar1c ((x:y:zs), acc)
 | x > y = headAcc (trocar1c ((x:zs), acc)) y
 | otherwise = headAcc (trocar1c  ((y:zs), acc)) x

bolhaOrd1c (lista,acc) 0 = (lista,acc)
bolhaOrd1c (lista,acc) n 
 | fst(trocar1c (lista,acc)) == fst (lista,acc) = (lista,acc)
 | otherwise = bolhaOrd1c (trocar1c (lista,acc)) (n-1)

bolha1c [] = ([],0)
bolha1c lista = bolhaOrd1c (lista,0) (length lista)

-- variação 2 com contador

appendx::  ([a],Int) -> [a] -> ([a],Int)
appendx ([], acc) y = (y, acc)
appendx ((x:xs),acc) y = ((x:xs) ++ y, acc)

trocar2c ([x],acc) = ([x],acc)
trocar2c ((x:y:zs), acc)
 | x > y = headAcc (trocar2c ((x:zs), acc)) y
 | otherwise = headAcc (trocar2c  ((y:zs), acc)) x

bolhaOrd2c:: (Ord a) => ([a],Int) -> Int -> ([a],Int)
bolhaOrd2c (lista,acc) 0 = (lista,acc)
bolhaOrd2c ([],acc) _ = ([],acc)
bolhaOrd2c (lista,acc) n = appendx (bolhaOrd2c (aux3,acc1) (n-1)) aux4
                      where 
                          aux  = trocar2c (lista,acc)
                          aux3 = take (n-1) (fst aux)
                          aux4 = drop (n-1) (fst aux)
                          acc1 = snd aux
                                     
bolha2c [] = ([],0)
bolha2c lista = bolhaOrd2c (lista,0) (length lista)

-- variação 3 com contador

trocar3c ([x],acc) = ([x],acc)
trocar3c ((x:y:zs), acc)
 | x > y = headAcc (trocar3c ((x:zs), acc)) y
 | otherwise = headAcc (trocar3c  ((y:zs), acc)) x

bolhaOrd3c:: (Ord a) => ([a],Int) -> Int -> ([a],Int)
bolhaOrd3c (lista,acc) 0 = (lista,acc)
bolhaOrd3c ([],acc) _ = ([],acc)
bolhaOrd3c (lista,acc) n 
 | fst aux == lista = (lista,acc)
 | otherwise = appendx (bolhaOrd3c (aux3,acc1) (n-1)) aux4
                      where 
                          aux  = trocar3c (lista,acc)
                          aux3 = take (n-1) (fst aux)
                          aux4 = drop (n-1) (fst aux)
                          acc1 = snd aux
                                     
bolha3c [] = ([],0)
bolha3c lista = bolhaOrd3c (lista,0) (length lista)

-- 1500500
-- 2001000
-- 4002000