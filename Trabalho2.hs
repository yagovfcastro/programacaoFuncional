
--Listas 
l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
l8 = l6++l6
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

--Avaliação das variações por número de comparações:

-- Podemos perceber a diferença de desempenho nos algoritmos conforme o número de comparações que cada um executa
-- Abaixo eles estão classificados por número de comparações (em ordem ascendente):
-- variação 3
-- variação 1
-- variação 2
-- original

--Avaliação das variações por tempo de execução:

-- Podemos, claramente, perceber uma diferença no tempo de execução de cada um dos algoritmos, principalmente quando
-- utilizamos uma lista grande, como, por exemplo, a l6.
-- Organizando-os por tempo de execução (em ordem ascendente):
-- variação 3
-- variação 1
-- variação 2
-- original

-- A vencedora foi: variação 3
-- A partir das avaliações, podemos concluir que, a variação 3 é a mais otimizada tanto em termos de tempo de execução
-- quanto em termos de números de comparação.

--4)

--quicksort original

quicksortOrg:: (Ord a) => [a] -> [a]
quicksortOrg [] = []
quicksortOrg (s:xs) = quicksortOrg [x | x <- xs, x < s]
                      ++ [s] ++
                      quicksortOrg [x | x <- xs, x >= s]

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

from3Median:: (Ord a) => [a] -> a
from3Median lst
 | length lst <= 2 = head lst
 | (x <= y && x >= z) || (x <= z && x >= y) = x
 | (y <= x && y >= z) || (y <= z && y >= x) = y
 | otherwise = z   
    where [x,y,z] = (take 3 lst)

delFirstOcur:: (Eq a) => a -> [a] -> [a] 
delFirstOcur _ [] = [] 
delFirstOcur z (x:xs) 
 | z == x    =  xs
 | otherwise = x : delFirstOcur z xs

quicksort2:: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 lista = quicksort2 (fst m1) ++ [pivo] ++ quicksort2 (snd m1)
                     where 
                        pivo = from3Median lista 
                        m2   = delFirstOcur pivo  lista 
                        m1   = divide pivo m2

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

--Avaliação das variações por número de comparações:

-- Podemos perceber a diferença de desempenho nos algoritmos conforme o número de comparações que cada um executa
-- Abaixo eles estão classificados por número de comparações (em ordem ascendente):
-- variação 2 
-- variação 1
-- original

--Avaliação das variações por tempo de execução:

-- Podemos, claramente, perceber uma diferença no tempo de execução de cada um dos algoritmos, principalmente quando
-- utilizamos uma lista grande, como, por exemplo, a l6.
-- Organizando-os por tempo de execução (em ordem ascendente):
-- variação 2
-- variação 1
-- original

-- A vencedora foi: variação 2
-- A partir das avaliações, podemos concluir que, a variação 2 é a mais otimizada tanto em termos de tempo de execução
-- quanto em termos de números de comparação.

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

--Essa operação pode ser dividida em 3 passos:
-- (eval (Add (Val 3) (Val 12)))
-- (eval (Sub (Val 15) (Val 5)))
-- (eval (Mul (Val 1) (Val 3)))

-- Resultando em:
resultado:: (Integral a) => a
resultado = eval (Mul (Val (eval (Add (Val 3) (Val 12)))) (Val (eval (Elv (Val (eval (Sub (Val 15) (Val 5)))) (Val (eval (Mul (Val 1) (Val 3)))))))) -- = 15000

--   - ((6+8-5+1)*(2+6^2))

--Essa operação pode ser dividida em 3 passos:
-- (eval (Add (Val (eval (Sub (Val (eval (Add (Val 6) (Val 8)))) (Val 5)))) (Val 1)))
-- (eval (Add (Val 2) (Val (eval (Elv (Val 6) (Val 2))))))
-- (eval (Mul (Val (eval (Add (Val (eval (Sub (Val (eval (Add (Val 6) (Val 8)))) (Val 5)))) (Val 1)))) (Val (eval (Add (Val 2) (Val (eval (Elv (Val 6) (Val 2)))))))))

resultado2:: (Integral a) => a
resultado2 = eval (Sub (Val 0) (Val (eval (Mul (Val (eval (Add (Val (eval (Sub (Val (eval (Add (Val 6) (Val 8)))) (Val 5)))) (Val 1)))) (Val (eval (Add (Val 2) (Val (eval (Elv (Val 6) (Val 2)))))))))))

--8)

data Aplicativo = Linkedin |
                  Whatsapp |
                  Facebook
            deriving(Show, Eq, Ord) 

data Contatotype =  Fone|
                    Nome
            deriving(Show, Eq, Ord)

data Ampm = AM |
            PM
        deriving(Show, Eq, Ord)

type Hora = (Ampm, Int, Int)
type Data = (Int, Int, Int)
type App = Aplicativo
type Contato = (Contatotype, String)

data TxtMsg = Msg { contato:: Contato,
                    msg:: String,
                    date:: Data,
                    horario:: Hora,
                    app:: App} deriving (Show, Eq, Ord)
 
m1  = Msg { contato = (Fone,"993842736"), msg = "Eh da emergencia?", date = (24, 09, 2020), horario = (PM, 02,21), app = Linkedin}
m2  = Msg { contato = (Fone,"846582301"), msg = "Ola! Sou eu, Joel", date = (24, 09, 2020), horario = (AM, 02, 22), app = Linkedin}
m3  = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (24, 09, 2020), horario = (AM, 02, 23), app = Linkedin}
m5  = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (24, 09, 2020), horario = (PM, 02, 24), app = Linkedin}
m4  = Msg { contato = (Nome,"Claudinho"), msg = "Ola! Sou eu, Claudinho", date = (24, 09, 2020), horario = (AM, 02, 25), app = Linkedin}
m6  = Msg { contato = (Nome,"Julia"), msg = "Ola! Sou eu, Julia", date = (24, 09, 2020), horario = (AM, 02, 26), app = Linkedin}
m7  = Msg { contato = (Nome,"Maria"), msg = "Ola! Sou eu, Maria", date = (24, 09, 2020), horario = (PM, 02, 27), app = Linkedin}
m8  = Msg { contato = (Fone,"937172373"), msg = "Tem cafe?", date = (24, 09, 2020), horario = (AM, 02, 28), app = Linkedin}
m9  = Msg { contato = (Nome,"Cleiton"), msg = "Ola! Sou eu, Cleiton", date = (24, 09, 2020), horario = (AM, 02, 29), app = Linkedin}
m10 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (24, 09, 2020), horario = (AM, 02, 30), app = Facebook}
m12 = Msg { contato = (Fone,"957187363"), msg = "Bateram na minha porta, foi voce?", date = (24, 09, 2020), horario = (AM, 02, 31), app = Facebook}
m11 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (24, 09, 2020), horario = (AM, 02, 32), app = Facebook}
m13 = Msg { contato = (Fone,"993842736"), msg = "Ola! Sou eu, Cleiton Rasta", date = (24, 09, 2020), horario = (AM, 02, 33), app = Facebook}
m14 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (24, 09, 2020), horario = (PM, 02, 34), app = Facebook}
m15 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (25, 09, 2020), horario = (AM, 02, 35), app = Facebook}
m16 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (25, 09, 2020), horario = (PM, 02, 36), app = Facebook}
m17 = Msg { contato = (Fone,"222222222"), msg = "Quanto que ta o peixe?", date = (25, 09, 2020), horario = (AM, 02, 37), app = Facebook}
m18 = Msg { contato = (Fone,"991374817"), msg = "10 reais o ovo, quer?", date = (25, 09, 2020), horario = (PM, 02, 38), app = Facebook}
m19 = Msg { contato = (Fone,"975817373"), msg = "Bom dia flor do dia", date = (25, 09, 2020), horario = (AM, 02, 39), app = Facebook}
m20 = Msg { contato = (Fone,"993842736"), msg = "Eu sou o batman", date = (25, 09, 2020), horario = (PM, 02, 40), app = Facebook}
m21 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (25, 09, 2020), horario = (AM, 02, 41), app = Whatsapp}
m22 = Msg { contato = (Nome,"Lucas"), msg = "Ola! Sou eu, Lucas", date = (25, 09, 2020), horario = (PM, 02, 42), app = Whatsapp}
m23 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (25, 09, 2020), horario = (AM, 02, 43), app = Whatsapp}
m24 = Msg { contato = (Fone,"947581727"), msg = "Ola! Ta quanto o pastel?", date = (25, 09, 2020), horario = (AM, 02, 44), app = Whatsapp}
m25 = Msg { contato = (Fone,"905716274"), msg = "Boa tarde Marilene", date = (25, 09, 2020), horario = (PM, 02, 45), app = Whatsapp}
m26 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (25, 09, 2020), horario = (AM, 02, 45), app = Whatsapp}
m27 = Msg { contato = (Nome,"Lucas"), msg = "Ola! Sou eu, Lucas", date = (25, 09, 2020), horario = (PM, 02, 46), app = Whatsapp}
m28 = Msg { contato = (Nome,"Jorge"), msg = "Ola! Sou eu, Jorge", date = (25, 09, 2020), horario = (AM, 02, 47), app = Whatsapp}
m29 = Msg { contato = (Fone,"938471723"), msg = "Alo, eh da joalheria?", date = (25, 09, 2020), horario = (AM, 02, 48), app = Whatsapp}
m30 = Msg { contato = (Fone,"917416726"), msg = "Sim, eu sou eu", date = (25, 09, 2020), horario = (PM, 02, 49), app = Whatsapp}

msgLst1:: [TxtMsg]
msgLst1 = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10]
msgLst2:: [TxtMsg]
msgLst2 = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20]
msgLst3:: [TxtMsg]
msgLst3 = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30]

--b

trocarMsg [x] = [x]
trocarMsg (x:y:zs)
 | fst(contato x) > fst(contato y)  = y : trocarMsg (x:zs)
 | fst(contato y) > fst(contato x)  = x : trocarMsg (y:zs)
 | (fst(contato x) == fst(contato y)) && (snd(contato x) < snd(contato y)) = x : trocarMsg (y:zs)
 | otherwise = y : trocarMsg (x:zs) 

bolhaOrdMsg lista 0 = lista
bolhaOrdMsg lista n = bolhaOrdMsg (trocarMsg lista) (n-1)

bolhaMsg [] = []
bolhaMsg lista = bolhaOrdMsg lista (length lista)

--c

divideData:: TxtMsg -> (Int,Int,Int)
divideData msg = (x,y,z)
          where (x,y,z) = date msg

divideHora:: TxtMsg -> (Ampm,Int,Int)
divideHora msg = (x,y,z)
          where (x,y,z) = horario msg

vemAntes:: TxtMsg -> TxtMsg -> Bool
vemAntes msg1 msg2   
 | (ano1 < ano2) || ((ano1 == ano2) && (mes1 < mes2)) || (((ano1 == ano2) && (mes1 == mes2)) && (dia1 < dia2)) = True
 | ((ano1 == ano2) && (mes1 == mes2) && (dia1 == dia2)) && (ampm1 < ampm2) = True
 | ((ano1 == ano2) && (mes1 == mes2) && (dia1 == dia2)) && ((ampm1 == ampm2) && (horas1 < horas2)) = True
 | ((ano1 == ano2) && (mes1 == mes2) && (dia1 == dia2)) && ((ampm1 == ampm2) && (horas1 == horas2)) && (minutos1 < minutos2) = True
 | otherwise = False 
   where
         (ampm1, horas1,minutos1) = divideHora msg1
         (dia1, mes1, ano1)       = divideData msg1
         (ampm2, horas2,minutos2) = divideHora msg2
         (dia2, mes2, ano2)       = divideData msg2  

quicksortMsg::[TxtMsg] -> [TxtMsg]
quicksortMsg [] = []
quicksortMsg (s:xs) = quicksortMsg [x | x <- xs, vemAntes x s]
                                   ++ [s] ++
                      quicksortMsg [x | x <- xs, not (vemAntes x s)]

--d

getLast:: [TxtMsg] -> [TxtMsg]
getLast [] = []
getLast [msg] = [msg]
getLast (x:xs)
 | length (x:xs) == 2 = (x:xs)
 | otherwise = getLast xs

getLstContato:: Contato -> [TxtMsg] -> [TxtMsg]
getLstContato cont lst = [x | x <- lst, contato x == cont] 

consulta2:: Contato -> [TxtMsg] -> [TxtMsg]
consulta2 cont lst =  result
        where 
              lstCont = getLstContato cont lst
              lstOrd  = quicksortMsg lstCont
              result  = getLast lstOrd

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
 | e1 == '/' = (evalTree e2) ^ (evalTree e3)
 | otherwise = error "Operador Inválido!"
