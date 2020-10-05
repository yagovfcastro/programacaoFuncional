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
               

 