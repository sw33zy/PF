--import Data.Either

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo a b 
                 |a > b = []
                 |otherwise = a : myEnumFromTo (a+1) b

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x y z
                       |(x > y) || (x > z) || (y > z) = []
                       |otherwise = x : myEnumFromThenTo y (y+y-x) z 

myplusplus :: [a] -> [a] -> [a]
myplusplus l [] = l
myplusplus [] l = l
myplusplus (x:xs) y = x : myplusplus xs y

myindex :: [Int] -> Int -> Int
myindex (x:xs) 0 = x
myindex (x:xs) a = myindex xs (a-1)

myreverse :: [Int] -> [Int]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: Int -> [Int] -> [Int]
mytake 0 l = []
mytake _ [] = []
mytake a (x:xs) = x : mytake (a-1) xs

mydrop :: Int -> [Int] -> [Int]
mydrop 0 l = l
mydrop _ [] = []
mydrop a (x:xs) = mydrop (a-1) xs

myzip :: [Int] -> [Int] -> [(Int,Int)]
myzip [] [] = []
myzip []  l = []
myzip l  [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

myelem :: Int -> [Int] -> Bool
myelem _ [] = False
myelem a (x:xs)
                | a == x = True
                | otherwise = myelem a xs

myreplicate :: Int -> Int -> [Int]
myreplicate x 0 = []
myreplicate x y = x : myreplicate x (y-1)

myintersperce :: Int -> [Int] -> [Int]
myintersperce _ [] = []
myintersperce a (x:xs) = x : a : myintersperce a xs

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (x : xs) = (aux1 x xs) : mygroup(drop(length(aux1 x xs)) (x:xs))

aux1 :: Eq a => a -> [a] -> [a]
aux1 a [] = [a]
aux1 a (x : xs) = if (a == x) then a : (aux1 x xs) else aux1 a []

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x : xs) = x ++ (myconcat xs)

myinits :: [a] -> [[a]]
myinits x = aux2 0 x

aux2 :: Int -> [a] -> [[a]]
aux2 i l 
          |i < length(l) = (take i l) : aux2 (i+1) l 
          |otherwise = [l]

mytails :: [a] -> [[a]]
mytails x = aux3 0 x

aux3 :: Int -> [a] -> [[a]]
aux3 i l 
         |i < length(l) = (drop i l) : aux3 (i+1) l
         | otherwise = [[]]

myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _ = True
myisPrefixOf _ [] = False
myisPrefixOf (x:xs) (y:ys) = if (x==y) then myisPrefixOf xs ys else False

myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = True
myisSuffixOf _ [] = False
myisSuffixOf (x:xs) (y:ys) = myisPrefixOf (reverse(x : xs)) (reverse(y:ys))

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False
myisSubsequenceOf (x:xs) (y:ys) 
                                | (x == y) = myisSubsequenceOf xs ys
                                | otherwise = myisSubsequenceOf (x:xs) ys


myelemIndices :: Eq a => a ->[a] -> [Int]
myelemIndices _ [] = []
myelemIndices a l = aux4 0 a l

aux4 :: Eq a => Int -> a -> [a] -> [Int]
aux4 _ _ [] = []
aux4 r u (x:xs)
               |(u == x) = r : aux4 (r+1) u xs
               |otherwise = aux4 (r+1) u xs


mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (aux6 x xs)

aux6 :: Eq a => a -> [a] -> [a]
aux6 _ [] = []
aux6 a (x:xs)
             | (a /= x) = x : aux6 a xs
             | otherwise = aux6 a xs

mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete a (x:xs) 
                  | (a == x) = xs
                  | otherwise = x : mydelete a xs  

myslashslash :: Eq a => [a] -> [a] -> [a]
myslashslash l [] = l
myslashslash [] s = []
myslashslash l (s : ss) = myslashslash (mydelete s l) ss

myunion :: Eq a => [a] -> [a] -> [a] 
myunion l [] = l
myunion [] s = s
myunion l s = l ++ myslashslash  s l

myintersect :: Eq a => [a] -> [a] -> [a] 
myintersect [] [] = []
myintersect [] l = []
myintersect (h:t) l = if ((aux7 h l)==True) then h:myintersect t l else myintersect t l 

aux7 :: Eq a => a -> [a] -> Bool
aux7 a [] = False
aux7 a (x : xs)
               | (a == x) = True
               | otherwise = aux7 a xs 

myinsert :: Ord a => a -> [a] -> [a]
myinsert a [] = [a]
myinsert a (x : xs) 
                    | (a >= x) = x : myinsert a xs
                    | otherwise = a : (x : xs)

myunwords :: [String] -> String
myunwords [] = []
myunwords [x] = x
myunwords (x : xs) = x ++ " " ++ myunwords xs

myunlines :: [String] -> String
myunlines [] = []
myunlines [x] = x
myunlines (x : xs) = x ++ "\n" ++ myunlines xs

pMaior :: Ord a => [a] -> Int
pMaior [] = 0
pMaior (x:xs) = indice 0 (maior x (x:xs)) (x:xs)

maior :: Ord a => a -> [a] -> a
maior a [] = a
maior a (x : xs)
                 | (a < x) = maior x xs
                 | otherwise = maior a xs

indice :: Eq a => Int -> a -> [a] -> Int
indice y a (x : xs)
                  | (a == x) = y
                  | otherwise = indice (y+1) a xs

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x : xs)
                     |(elem x xs == True) = True
                     |otherwise = temRepetidos xs

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x : xs)
                    | (x == '1') = x : algarismos xs
                    | (x == '2') = x : algarismos xs
                    | (x == '3') = x : algarismos xs
                    | (x == '4') = x : algarismos xs
                    | (x == '5') = x : algarismos xs
                    | (x == '6') = x : algarismos xs
                    | (x == '7') = x : algarismos xs
                    | (x == '8') = x : algarismos xs
                    | (x == '9') = x : algarismos xs
                    | (x == '0') = x : algarismos xs
                    | otherwise = algarismos xs

posImpares ::  [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x : xs) = (head xs) : posImpares (tail xs)

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x : xs) = x : posPares (tail xs)
     
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : xs)
                  | (x > (head xs)) = False
                  | otherwise = isSorted xs

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort l = aux8 l [] 

aux8 :: Ord a => [a] -> [a] -> [a]
aux8 [] l = l
aux8 (x : xs) l = aux8 xs (myinsert x l)

menor :: String -> String -> Bool
menor "" "" = False
menor "" s = True
menor s "" = False
menor (x : xs) (y : ys) = menor xs ys

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((h,t) : ms) 
                        | (a == h) = True
                        | otherwise = elemMSet a ms

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((h,t) : ms) = t+(lengthMSet ms)

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h,t):ms) = aux9 0 t h ++ converteMSet ms

aux9 :: Int -> Int -> a -> [a]
aux9 x y a
           | (x < y) = a : aux9 (x+1) y a
           | otherwise = []

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a, 1)]
insereMSet a ((h,t) : ms)
                         |(a == h) = ((h,t+1) : ms)
                         |otherwise = (h,t) : insereMSet a ms

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((h,t) : ms)
                         |((a == h) && ( t > 1)) = (h,t-1) : removeMSet a ms
                         |((a == h) && (t == 1)) = removeMSet a ms
                         |otherwise = (h,t) : removeMSet a ms

 
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x : xs) = aux10 x (mygroup (x:xs)) ++ constroiMSet (deletereps x xs)
                      

aux10 :: Eq a => a -> [[a]] -> [(a,Int)]
aux10 _ [] = []
aux10 a (x : xs)
                 | (a == head x) = (a, length x) : aux10 a xs
                 |otherwise = aux10 a xs

deletereps :: Eq a => a -> [a] -> [a]
deletereps _ [] = []
deletereps a (x:xs)
                    | (a == x) = deletereps a xs
                    | otherwise = x : deletereps a xs

myPartitionEithers :: [Either a b] -> ([a],[b])
myPartitionEithers l = (leftg l, rightg l)

leftg :: [Either a b] -> [a]
leftg [] = []
leftg (x:xs)
             |(isLeft x) = fromEitherL x : leftg xs
             |otherwise = leftg xs

rightg :: [Either a b] -> [b]
rightg [] = []
rightg (x:xs) 
             |(isRight x) = fromEitherR x : rightg xs
             |otherwise = rightg xs

isLeft :: Either a b -> Bool
isLeft (Left a) = True
isLeft (Right b) = False

isRight :: Either a b -> Bool
isRight (Left a) = False
isRight (Right b) = True 

fromEitherL :: Either a b -> a
fromEitherL (Left a) = a

fromEitherR :: Either a b -> b
fromEitherR (Right b) = b

mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = []
mycatMaybes (x : xs)
                     |(myisJust x) = myfromJust x : mycatMaybes xs
                     |otherwise = mycatMaybes xs

myisJust :: Maybe a -> Bool
myisJust (Just a) = True
myisJust Nothing = False

myfromJust :: Maybe a -> a
myfromJust (Just a) = a 


data Movimento = Norte | Sul | Este | Oeste deriving Show 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (z:zs)
                    |(isNorte z) = posicao (x, y+1) zs
                    |(isSul z) = posicao (x, y-1) zs
                    |(isEste z) = posicao (x+1, y) zs
                    |(isOeste z) = posicao (x-1, y) zs

isNorte :: Movimento -> Bool
isNorte (Norte) = True
isNorte (Sul) = False
isNorte (Este) = False
isNorte (Oeste) = False

isSul :: Movimento -> Bool
isSul (Norte) = False
isSul (Sul) = True
isSul (Este) = False
isSul (Oeste) = False

isEste :: Movimento -> Bool
isEste (Norte) = False
isEste (Sul) = False
isEste (Este) = True
isEste (Oeste) = False

isOeste :: Movimento -> Bool
isOeste (Norte) = False
isOeste (Sul) = False
isOeste (Este) = False
isOeste (Oeste) = True

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (w,z)
                   |((w > x) && (z > y)) = Este : Norte : caminho (x+1,y+1) (w,z)
                   |((w < x) && (z < y)) = Oeste : Sul : caminho (x-1,y-1) (w,z)
                   |(w > x) = Oeste : caminho (x+1,y) (w,z)
                   |(w < x) = Este : caminho (x-1,y) (w,z)
                   |(z > y) = Norte : caminho (x,y+1) (w,z)
                   |(z < y) = Sul : caminho (x,y-1) (w,z)
                   |otherwise = []

vertical :: [Movimento] -> Bool
vertical [Norte] = True
vertical [Sul] = True
vertical [] = False
vertical (x:xs)
                |((isEste x) || (isOeste x)) = False
                |otherwise = vertical xs

data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral (x : xs) = aux11 x xs

aux11 :: Posicao -> [Posicao] -> Posicao
aux11 a [] = a
aux11 a (x:xs)
               |(sumPosicao(a)  > sumPosicao(x)) = aux11 x xs
               |otherwise = aux11 a xs

sumPosicao :: Posicao -> Int
sumPosicao ( Pos x y) = (abs(x)+abs(y))


vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos a (x:xs)
                 |((vizVertical a x) || (vizHorizontal a x)) = x : vizinhos a xs
                 | otherwise = vizinhos a xs

vizHorizontal :: Posicao -> Posicao -> Bool
vizHorizontal (Pos x y) (Pos w z)
                               |((w == (x+1)) || (w == (x-1))) = True
                               |otherwise = False

vizVertical :: Posicao -> Posicao -> Bool
vizVertical (Pos x y) (Pos w z)
                                 |((z == y+1) || (z == y-1)) = True
                                 |otherwise = False

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada (x:xs) = aux12 x xs

aux12 :: Posicao -> [Posicao] -> Bool
aux12 a [x]
           |(ordenada a == ordenada x) = True
           |otherwise = False

aux12 a (x:xs)
              |(ordenada a == ordenada x) = aux12 a xs
              |otherwise = False

ordenada :: Posicao -> Int
ordenada (Pos x y) = y

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l
                |(numerosemaforosnaovermelhos l 0 > 1) = False
                |otherwise = True

numerosemaforosnaovermelhos :: [Semaforo] -> Int -> Int
numerosemaforosnaovermelhos [] i = i
numerosemaforosnaovermelhos (x : xs) i
                                       |(isVermelho x) = numerosemaforosnaovermelhos xs i
                                       |otherwise = numerosemaforosnaovermelhos xs i+1

isVermelho :: Semaforo -> Bool
isVermelho (Vermelho) = True
isVermelho (Verde) = False
isVermelho (Amarelo) = False
