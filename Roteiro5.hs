



--Heya there, just trying to learn git through this, kekw





--3)

--1) (λ x.22*x + 1) 3 ==> 2*3 + 1 == 7
--2) (λ xy.x-y) 5 7 ==> 5-7 == -2
--3) (λ yx.x-y) 5 7 ==> 7-5 == 2
--4) (λ xy.x-y) (λz.z/2) ==> isso vai resultar em um erro por falta de parâmetros, no caso tem um só
--5) (λ xy.x-y) ((λz -> z/2)6)1 ==> (λ xy.x-y) (6/2)1 ==> (λ xy.x-y) 3 1 ==> 3 - 1 == 2.0
--6) (λ x.λ y. - x y) 9 4 => (λ x y. - x y) 9 4 ==> (-) 9 4 == 5
--7) (λ x.xx) (λ y.y) ==> sinceramente, não sei 

--4)

-- (\x -> x + 3) 5 == 8
-- (\x -> \y -> x * y + 5) 3 4 == 17
-- (\(x,y) -> x * y^2) (3,4) == 48
-- (\(x,y,_) -> x * y^2) (3,4,2) == 48
-- (\xs -> zip xs [1,2,3]) [4,5,6] = [(4,1),(5,2),(6,3)]

--5)

--a) (\x y -> y)((\z -> z)(\z -> z))(\w -> w) 5 == 5
--b) ((\f -> (\x -> f(f x)))(\y -> (y * y))) 3 == 81
--c) ((\f -> (\x -> f(f x)))(\y -> (y + y))) 5 == 20
--d) ((\x -> (\y -> x + y)5)((\y -> y - 3)7)) == 9
--e) (((\f -> (\x -> f(f(f x))))(\y -> (y * y)))2) == 256
--f) (\x y -> x + ((\x -> x - 3)y)) 5 6 == 8