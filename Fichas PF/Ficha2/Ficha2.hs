import Data.Char

--2ºa
dobro' :: [Float] -> [Float]
dobro' [] = []
dobro' (h:t) = h+h : dobro' t

--2ºb
numOcorre' :: Char -> String -> Int
numOcorre' x [] = 0
numOcorre' x (h:t) = if x==h then 1+numOcorre' x t else numOcorre' x t

--2ºc
positivos' :: [Int] -> Bool
positivos' [] = True
positivos' (h:t) = if h>0 && positivos' t then True else False

--2ºd
soPos' :: [Int] -> [Int]
soPos' [] = []
soPos' (h:t) = if h>0 then h : soPos' t else soPos' t

--2ºe
somaNeg' :: [Int] -> Int
somaNeg' [] = 0
somaNeg' (h:t) = if h<0 then h + somaNeg' t else somaNeg' t

--2ºf
tresUlt' :: [a] -> [a]
tresUlt' (h:t) | length (h:t) <= 3 = (h:t)
               | length (h:t) > 3 = tresUlt' t

--2ºg
segundos' :: [(a,b)] -> [b]
segundos' [] = []
segundos' ((x,y):t) = y : segundos' t

--2ºh
nosPrimeiros' :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros' x [] = False
nosPrimeiros' x ((h1,h2):t) = if x==h1 then nosPrimeiros' x t else False

--2ºi
sumTriplos' :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos' [(a,b,c)] = (a,b,c)
sumTriplos' ((a,b,c):t) = sumTriplos_aux (a,b,c) (sumTriplos' t)

sumTriplos_aux :: (Num a, Num b, Num c) => (a,b,c) -> (a,b,c) -> (a,b,c)
sumTriplos_aux (a1,a2,a3) (b1,b2,b3) = (a1+b1,a2+b2,a3+b3)

--3ºa
soDigitos' :: [Char] -> [Char]
soDigitos' [] = []
soDigitos' (h:t) = if isDigit h then h : soDigitos' t else soDigitos' t

--3ºb
minusculas' :: [Char] -> Int
minusculas' [] = 0
minusculas' (h:t) = if isLower h then 1 + minusculas' t else minusculas' t


--4º
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--4ºa
conta' :: Int -> Polinomio -> Int
conta' n [] = 0
conta' n ((h1,h2):t) = if n==h2 then 1 + conta' n t else conta' n t

--4ºb
grau' :: Polinomio -> Int
grau' [] = 0
grau' [(b1,b2)] = b2
grau' ((h1,h2):(a1,a2):t) = if h2 >= a2 then grau' ((h1,h2):t) else grau' ((a1,a2):t)

--4ºc
selgrau' :: Int -> Polinomio -> Polinomio
selgrau' x [] = []
selgrau' x ((h1,h2):t) = if x==h2 then (h1,h2) : selgrau' x t else selgrau' x t

--4ºd
deriv' :: Polinomio -> Polinomio
deriv' [] = []
deriv' ((h1,h2):t) = (h1*fromIntegral h2,h2-1) : deriv' t


--4ºe
calcula' :: Float -> Polinomio -> Float
calcula' x [] = 0
calcula' x ((h1,h2):t) = (h1*(x^h2)) + calcula' x t

--4ºf
simp' :: Polinomio -> Polinomio
simp' [] = []
simp' ((h1,h2):t) = if h1/=0 then (h1,h2) : simp' t else simp' t

--4ºg
mult' :: Monomio -> Polinomio -> Polinomio
mult' (x,y) [] = []
mult' (x,y) ((h1,h2):t) = (x*h1,y*h2) : mult' (x,y) t

--4ºh
normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' (h:t) = aux h (normaliza' t)
             
aux :: Monomio -> Polinomio -> Polinomio
aux (x,y) [] = [(x,y)]
aux (x,y) ((x1,y1):t) = if (y==y1) then ((x+x1),y) :t else (x1,y1) : (aux (x,y) t)

--4ºi
soma' :: Polinomio -> Polinomio -> Polinomio
soma' [] [] = []
soma' ((h1,h2):t) [] = ((h1,h2):t)
soma' [] ((a1,a2):t) = ((a1,a2):t)
soma' ((h1,h2):t) ((a1,a2):t1) = let y = ((h1,h2):t) ++ ((a1,a2):t1) 
                                     in normaliza' y

--4ºj
produto' :: Polinomio -> Polinomio -> Polinomio
produto' [] [] = []
produto' ((h1,h2):t) [] = []
produto' [] ((a1,a2):t) = []
produto' ((h1,h2):t) ((a1,a2):t1) = let w = produto_aux (h1,h2) ((a1,a2):t1) ++ produto' t ((a1,a2):t1)
                                        in normaliza' w

produto_aux :: Monomio -> Polinomio -> Polinomio
produto_aux (h1,h2) [] = []
produto_aux (h1,h2) ((a1,a2):t1) = (h1*a1,h2+a2) : produto_aux (h1,h2) t1

-- 4ºk
ordena' :: Polinomio -> Polinomio
ordena' [] = []
ordena' ((h1,h2):t) = insere' (h1,h2) (ordena' t)
                    where 
                          insere' x [] = [x]
                          insere' (h1,h2) ((h3,h4):t) | h2==h4 = (h1+h3,h2):t
                                                      | h2>h4 = (h1,h2):(h3,h4):t
                                                      | h2<h4 = (h1,h4):(insere' (h1,h2) t)

--4ºl
equiv' :: Polinomio -> Polinomio -> Bool
equiv' [] [] = True
equiv' [x] [] = False
equiv' [] [x] = False
equiv' ((h1,h2):t) (h:t1) = if equiv_aux (h1,h2) (h:t1) == True then equiv' t (filter (/=(h1,h2)) (h:t1)) else False

equiv_aux :: Monomio -> Polinomio -> Bool
equiv_aux (h1,h2) [] = False
equiv_aux (h1,h2) ((a1,a2):t1) = if h1==a1 && h2==a2 then True else equiv_aux (h1,h2) t1



