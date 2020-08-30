--1)

dobro:: Int -> Int
dobro x = x + x

quadruplo:: Int -> Int
quadruplo y = dobro (dobro y)

hipotenusa:: Float -> Float -> Float
hipotenusa x y = sqrt ((x * x) + (y * y))

distancia:: Float -> Float -> Float -> Float -> Float
distancia a b c d =  sqrt ((c - a)^2 + (d - b)^2)

--2)

-- fst (2,5) == 2
-- snd (5, "Bom dia") = "Bom dia"
-- (1,1) == (1,1) == True
-- (1,1) /= (1,1) == False
-- (1,1) < (1,2) == True
-- (2,1) < (1,2) == False
-- (1,2,3) < (1,2) == Erro de type mismatch
-- "azul" < "verde" == True
-- "azul" < "amarelo" == False
-- (1,2,3) == (,,) 1 2 3 == True

--3)

convertion:: Float -> (Float, Float, Float)
convertion real = (real, real * 3.96, real * 4.45)

--4)

bissexto:: Int -> Bool
bissexto ano 
    | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
    | otherwise = False

--5)

type Data = (Int, Int, Int)

bissexto2:: Data -> Bool
bissexto2 (_,_,x) = if bissexto x then True
                                  else False  

--6)

valida:: Data -> Bool
valida (x, y, z) 
    | bissexto z && (y == 2 && x >= 1 && x <= 29) && z > 0 = True  
    | (y == 2 && x >= 1 && x <= 28) && z > 0 = True
    | (x >= 1 && x <= 30) && (y == 4 || y == 6 || y == 9 || y == 11) && z > 0 = True
    | (x >= 1 && x <= 31) && (y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 12) && z > 0 = True
    | otherwise = False

--7)

precede:: Data -> Data -> Bool
precede (a1, a2, a3) (b1, b2, b3)
    | b3 > a3 = True
    | b3 == a3 && (b2 > a2 || (b2 == a2 && b1 > a1)) = True
    | otherwise = False

--8)

-- código do livro, título do livro, autor, editora e ano de publicação
type Livro = (String, String, String, Int) 

-- código do aluno, nome, e-mail e telefone
type Pessoa = (String, String, String, String)

-- código do livro, código do aluno, data de empréstimo, data de devolução e situação
type Emprestimo = (String, String, Data, Data, String)

--9)

e1:: Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

e2:: Emprestimo
e2 = ("H123C9","BSI200945",(06,8,2009),(20,8,2021),"aberto")

dataExemplo:: Data
dataExemplo = (21,8,2020)

-- Utilizei um retorno de String porque fica mais visualmente agradável do que o retorno em Booleano, espero que não tenha problema ^_^
verificaEmprestimo:: Data -> Emprestimo -> String
verificaEmprestimo dataAtual (_,_,_,dataDevolucao,_) = if precede dataDevolucao dataAtual then "Atrasado"
                                                                                            else "Dia da entrega ainda nao chegou"

-- mdc:: (Int, Int) -> Int
-- mdc(m,n)
-- 	| n == 0 = m
-- 	| otherwise = mdc(n, (m mod n))

-- mdc:: (Int, Int) -> Int
-- mdc(m,0) = m
-- mdc(m,n) = mdc(n, (m mod n)

-- gerapalindrome n = [1..n] ++ reverse [1..n]