--1)
-- 1 + 2 * 3 == 7
-- 5 ^ 3 == 125
-- 5 ** 3 == 125.0
-- 5 / 3 == 1.6666666666666667
-- div 5 3 == 1
-- mod 5 3 == 2
-- 5 < 3 == False
-- mod 5 3 < 2 == False
-- mod 5 3 == 2
-- sqrt 81 == 9.0
-- logBase 2 1024 == 10.0
-- floor 5.7 == 5
-- ceiling 5.7 == 6
-- abs (-5) == 5
-- min 6 7 == 6
-- max 6 7 == 7
-- sin (pi/2) == 1.0
-- sum [1..5] == 15
-- not True == False
-- True && False == False

--2)
dobro x = x + x

--3)
quadruplo y = dobro (dobro y)

--4)
hipotenusa x y = sqrt ((x * x) + (y * y))

--5)
distancia a b c d =  sqrt ((c - a)^2 + (d - b)^2)