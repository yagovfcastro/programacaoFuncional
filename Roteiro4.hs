
--1)

lst1 = [x*2 | x <- [1..10], x*2 >= 12] --  == [12,14,16,18,20]
lst2 = [ x | x <- [50..100], mod x 7 == 3] -- == [52,59,66,73,80,87,94]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] -- == [10,11,12,14,16,17,18,20]
lst4=[(x,y)| x <- [1..4], y <- [x..5]] -- == [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]

--2)

quadrados:: Int -> Int -> [Int]
quadrados x y = [i*i | i <- [x..y]]

--3)

selecionaImpares:: [Int] -> [Int]
selecionaImpares x = [i | i <- x, i `mod` 2 /= 0]

--4)

tabuada:: Int -> [Int]
tabuada x = [x * i | i <- [1..10]]

--5)

bissexto:: Int -> Bool
bissexto ano 
    | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
    | otherwise = False

bissextos:: [Int] -> [Int]
bissextos x = [i | i <- x, bissexto i]

--6)

sublistas:: [[Int]] -> [Int]
sublistas x = [i | j <- x, i <- j]

--7)

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
                ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
                ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede:: Data -> Data -> Bool
precede (a1, a2, a3) (b1, b2, b3)
    | b3 > a3 = True
    | b3 == a3 && (b2 > a2 || (b2 == a2 && b1 > a1)) = True
    | otherwise = False   

verificaEmprestimo:: Data -> Emprestimo -> Bool
verificaEmprestimo dataAtual (_,_,_,dataDevolucao,_) = if precede dataDevolucao dataAtual then False
                                                                                            else True 

atrasados:: Emprestimos -> Data -> Emprestimos
atrasados emp date = [i | i <- emp, not (verificaEmprestimo date i)]

--8)

npares:: [Int] -> Int
npares [] = 0
npares (x:y) = if x `mod` 2 == 0 then 1 + npares y else 0 + npares y

--9)

produtorio:: [Int] -> [Int]
produtorio x = [i * j | i <- x, j <- x]

--10)

comprime:: [[a]] -> [a]
comprime [] = [] 
comprime (x:y) = x ++ comprime y

--11)

tamanho:: [a] -> Int
tamanho [] = 0
tamanho (x:y) = 1 + tamanho y

--12)

uniaoNRec:: [Int] -> [Int] -> [Int]
uniaoNRec x y = x ++ [i | i <- y, not (i `elem` x)]  

--13)

uniaoNRec2:: [Int] -> [Int] -> [Int]
uniaoNRec2 x [] = x
uniaoNRec2 x (w:z) = if w `elem` x then uniaoNRec2 x z else uniaoNRec2 (x ++ [w]) z