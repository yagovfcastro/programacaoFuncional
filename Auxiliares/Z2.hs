l1 = [1..1000]
l2 = [1000,999..1]
l3 = l1++[0]
l4 = [0]++l2
l5 = l1++[0]++l2
l6 = l2++[0]++l1
l7 = l2++[0]++l2
x1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]


--Com acumulador de comparações

-- original com contador

ordLeft ([],acc) _ = ([],acc)
ordLeft ((x:xs),acc) pivo 
 | x < pivo  = headAcc (ordLeft(xs,acc) pivo) x
 | otherwise = ordLeft (xs, acc+1) pivo

ordRight ([],acc) _ = ([],acc)
ordRight ((x:xs),acc) pivo 
 | x >= pivo  = headAcc (ordRight(xs,acc) pivo) x
 | otherwise  = ordRight (xs, acc+1) pivo

quicksortCont:: (Ord a) => [a] -> ([a], Int)
quicksortCont [] = ([], 0)
quicksortCont (s:xs) = (tl ++ [s] ++ tr, acc1 + acc2 + acc3 + acc4)
  where 
        (l,acc1)   = ordLeft  (xs, 0) s
        (r,acc2)   = ordRight (xs, 0) s
        (tl, acc3) = quicksortCont l
        (tr, acc4) = quicksortCont r

--variação 1 com contador

headAccL (l, r, acc) x = (x:l, r, acc+1)
headAccR (l, r, acc) x = (l, x:r, acc+1)

divideCont:: (Ord a) => a -> ([a], Int) -> ([a], [a], Int)
divideCont _ ([],acc) = ([], [], acc)
divideCont j ((x:xs),acc)
  | x < j     = headAccL (divideCont j (xs, acc)) x
  | otherwise = headAccR (divideCont j (xs, acc)) x
         
quicksortCont1:: (Ord a) => [a] -> ([a], Int)
quicksortCont1 [] = ([], 0)
quicksortCont1 (s:xs) = (tl ++ [s] ++ tr, acc1 + acc2 + acc3)
                     where 
                           (l, r, acc1) = divideCont s (xs, 0) 
                           (tl, acc2)   = quicksortCont1 l
                           (tr, acc3)   = quicksortCont1 r

--variação 2 com contador

from3MedianCont:: (Ord a) => [a] -> (a, Int)
from3MedianCont lst
 | length lst <= 2 = (head lst, 1) 
 | (x <= y && x >= z) || (x <= z && x >= y) = (x, 1)
 | (y <= x && y >= z) || (y <= z && y >= x) = (y, 1)
 | otherwise = (z, 1)    
    where [x,y,z] = (take 3 lst)

delFirstOcurCont:: (Eq a) => a -> ([a], Int)-> ([a], Int) 
delFirstOcurCont _ ([],acc) = ([],acc) 
delFirstOcurCont z ((x:xs), acc) 
 | z == x    =  (xs, acc + 1)
 | otherwise = headAcc (delFirstOcurCont z (xs, acc)) x

quicksortCont2:: (Ord a) => [a] -> ([a], Int)
quicksortCont2 [] = ([], 0)
quicksortCont2 lista = (tl ++ [pivo] ++ tr, acc1 + acc2 + acc3 + acc4 + acc5)
                     where 
                          (pivo, acc1)      = from3MedianCont lista
                          (firstocur, acc2) = delFirstOcurCont pivo (lista, 0) 
                          (l, r, acc3)      = divideCont pivo (firstocur, 0) 
                          (tl, acc4)        = quicksortCont2 l
                          (tr, acc5)        = quicksortCont2 r
                      
                        