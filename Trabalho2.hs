
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

--Com acumulador de comparações

-- original com contador

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

--Avaliações comparativas das variações

--Avaliação comparativa das variações por número de comparações:

-- Podemos perceber a diferença de desempenho nos algoritmos conforme o número de comparações que cada um executa
-- Abaixo eles estão classificados por número de comparações em ordem ascendente:
-- variação 3
-- variação 1
-- variação 2
-- original

--Avaliação comparativa das variações por tempo de execução:

-- Podemos, claramente, perceber uma diferença no tempo de execução de cada um dos algoritmos, principalmente quando
-- utilizamos uma lista grande, como, por exemplo, a l6.
-- Organizando-os por tempo de execução (em ordem ascendente):
-- variação 3
-- variação 1
-- variação 2
-- original

--A vencedora foi: variação 3
-- A partir das avaliações, podemos concluir que, a variação 3 é a mais otimizada tanto em termos de tempo de execução
-- quanto em termos de números de comparação.

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



--6)

--a

data Expr a = Val a |
            Add (Expr a) (Expr a) |
            Sub (Expr a) (Expr a) |
            Mul (Expr a) (Expr a) |
            Elv (Expr a) (Expr a)

eval::(Integral a) => Expr a -> a
eval (Val n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Elv e1 e2) = (eval e1) ^ (eval e2)

--b

--    (3+12)*(15-5)^(1*3)

-- Essa operação pode ser dividida em 3 passos:
-- (eval (Add (Val 3) (Val 12)))
-- (eval (Sub (Val 15) (Val 5)))
-- (eval (Mul (Val 1) (Val 3)))

-- Resultando em:
resultado:: (Integral a) => a
resultado = eval (Mul (Val (eval (Add (Val 3) (Val 12)))) (Val (eval (Elv (Val (eval (Sub (Val 15) (Val 5)))) (Val (eval (Mul (Val 1) (Val 3)))))))) -- = 15000

--   - ((6+8-5+1)*(2+6^2))

-- Essa operação pode ser dividida em 3 passos:
--(eval (Add (Val (eval (Sub (Val (eval (Add (Val 6) (Val 8)))) (Val 5)))) (Val 1)))
--(eval (Add (Val 2) (Val (eval (Elv (Val 6) (Val 2))))))
--(eval (Mul (Val (eval (Add (Val (eval (Sub (Val (eval (Add (Val 6) (Val 8)))) (Val 5)))) (Val 1)))) (Val (eval (Add (Val 2) (Val (eval (Elv (Val 6) (Val 2)))))))))

resultado2:: (Integral a) => a
resultado2 = eval (Sub (Val 0) (Val (eval (Mul (Val (eval (Add (Val (eval (Sub (Val (eval (Add (Val 6) (Val 8)))) (Val 5)))) (Val 1)))) (Val (eval (Add (Val 2) (Val (eval (Elv (Val 6) (Val 2)))))))))))

--8)



--10)

data ArvBinEA a = Vazia |
                  Folha a |
                  NoEA (Char, ArvBinEA a, ArvBinEA a)
                    deriving (Show)

ea::ArvBinEA Float
ea = (NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7))

evalTree:: (Integral a) => ArvBinEA a -> a
evalTree (Vazia) = 0 
evalTree (Folha n) = n
evalTree (NoEA (e1,e2,e3))
 | e1 == '+' = (evalTree e2) + (evalTree e3)
 | e1 == '-' = (evalTree e2) - (evalTree e3)
 | e1 == '*' = (evalTree e2) * (evalTree e3)
 | e1 == '^' = (evalTree e2) ^ (evalTree e3)
 | otherwise = error "Operador Inválido!"
