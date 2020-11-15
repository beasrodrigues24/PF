import Data.Char 
import Data.List 

-- Exercício 1
enumfromTo :: Int -> Int -> [Int] 
enumfromTo x y | x <= y = x : enumfromTo (x+1) y 
               | x > y = [] 

-- Exercício 2 
enumfromThenTo :: Int -> Int -> Int -> [Int]
enumfromThenTo x y z | (x < y && x <= z) || (x > y && x >= z) = x: enumfromThenTo y (2*y-x) z
                     | otherwise = [] 

-- Exercício 3 
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l 
(+++) l [] = l 
(+++) (h:t) l = h: (+++) t l 

-- Exercício 4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h 
(!!!) (h:t) n = (!!!) t (n-1) 

-- Exercício 5
reverse' :: [a] -> [a] 
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

-- Exercício 6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (h:t) | n <= 0 = [] 
              | otherwise = h: take' (n-1) t 

-- Exercício 7
drop' :: Int -> [a] -> [a] 
drop' _ [] = [] 
drop' n l@(h:t) | n > 0 = drop' (n-1) t  
                | otherwise = l

-- Exercício 8
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys
zip' _ _ = [] 

-- Exercício 9
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (h:t) = x == h || elem' x t 

-- Exercício 10
replicate' :: Int -> a -> [a]
replicate' n x | n > 0 = x: replicate' (n-1) x 
               | otherwise = [] 

-- Exercício 11
intersperse' :: a -> [a] -> [a]
intersperse' _ [n] = [n]
intersperse' x (h:t) = h: x: intersperse' x t
intersperse' _ l = l 

-- Exercício 12
group' :: Eq a => [a] -> [[a]]
group' (x:y:t) | x == y = ([x] ++ head (afterGrouping)) : tail (afterGrouping) 
               | otherwise = [x] : (group' (y:t))
    where afterGrouping = group' (y:t)
group' l = [l]

-- Exercício 13
concat' :: [[a]] -> [a] 
concat' [[]] = [] 
concat' (h:t) = h ++ concat' t

-- Exercício 14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits'(init l) ++ [l]

-- Exercício 15
tails' :: [a] -> [[a]] 
tails' [] = [[]]
tails' l = [l] ++ tails'(tail l)

-- Exercício 16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True 
isPrefixOf' _ [] = False 
isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys 
                          | otherwise = False

-- Exercício 17 
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True 
isSuffixOf' _ [] = False 
isSuffixOf' xs ys | last xs == last ys = isSuffixOf' (init(xs)) (init(ys))
                  | otherwise = False

-- Exercício 18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] l = True 
isSubsequenceOf' l [] = False 
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys
                               | otherwise = isSubsequenceOf' (x:xs) ys

-- Exercício 19
auxConta :: Eq a => Int -> a -> [a] -> [Int]
auxConta _ _ [] = [] 
auxConta n x (h:t) | x == h = n: auxConta (n+1) x t 
                   | otherwise = auxConta (n+1) x t

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x l = auxConta 0 x l

-- Exercício 20
apagaOcorrencias :: Eq a => a -> [a] -> [a]
apagaOcorrencias _ [] = [] 
apagaOcorrencias x (h:t) | x == h = apagaOcorrencias x t 
                         | otherwise = h: apagaOcorrencias x t

nub' :: Eq a => [a] -> [a] 
nub' [] = []
nub' (h:t) = h: nub'(apagaOcorrencias h t)

-- Exercício 21
delete' :: Eq a => a -> [a] -> [a]
delete' x [] = [] 
delete' x (h:t) | x == h = t 
                | otherwise = h: delete' x t

-- Exercício 22
(\\\):: Eq a => [a] -> [a] -> [a] 
(\\\) l [] = l 
(\\\) [] l = [] 
(\\\) l (h:t) = (\\\) (delete' h l) t

-- Exercício 23 
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l 
union' l (h:t) | elem h l = union' l t 
               | otherwise = union' (l ++ [h]) t 

-- Exercício 24 
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = [] 
intersect' l [] = [] 
intersect' (h:t) l | elem h t = h: intersect' t l 
                   | otherwise = intersect' t l

-- Exercício 25 
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x] 
insert' x (h:t) | x > h = h: insert' x t 
                | otherwise = x:h:t

-- Exercício 26
unwords' :: [String] -> String 
unwords' [] = ""
unwords' (h:t) = h ++ [' '] ++ unwords' t 

-- Exercício 27 
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ ['\n'] ++ unlines' t

-- Exercício 28
auxMaior :: Ord a => Int -> Int -> a -> [a] -> Int
auxMaior i n x [] = i
auxMaior i n x (h:t) | h > x = auxMaior n (n+1) h t 
                     | otherwise = auxMaior i (n+1) x t 

pMaior :: Ord a => [a] -> Int
pMaior (h:t) = auxMaior 0 1 h t

-- Exercício 29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False 
temRepetidos (h:t) = elem h t || temRepetidos t 

-- Exercício 30 
algarismos :: [Char] -> [Char]
algarismos [] = [] 
algarismos (h:t) | '1' <= h && h <= '9' = h: algarismos t 
                 | otherwise = algarismos t 

-- Exercício 31
posImpares :: [a] -> [a] 
posImpares (x:y:t) = y: posImpares t
posImpares _ = []

-- Exercício 32 
posPares :: [a] -> [a] 
posPares (x:y:t) = x: posPares t 
posPares l = l

-- Exercício 33 
isSorted :: Ord a => [a] -> Bool 
isSorted (x:y:t) | x <= y = isSorted (y:t)
                 | otherwise = False 
isSorted _ = True

-- Exercício 34
iSort :: Ord a => [a] -> [a]
iSort [] = [] 
iSort (h:t) = insert' h (iSort t)

-- Exercício 35
menor :: String -> String -> Bool 
menor [] l = True
menor l [] = False 
menor (x:xs) (y:ys) | x == y = menor xs ys 
                    | x < y = True 
                    | x > y = False 

-- Exercício 36 
elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet x [] = False 
elemMSet x ((y,n):t) | x == y = True 
                     | otherwise = elemMSet x t 

-- Exercício 37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,n):t) = n + lengthMSet t 

-- Exercício 38
expande :: (a,Int) -> [a] 
expande (x,0) = [] 
expande (x,n) = x: expande(x,n-1)

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = [] 
converteMSet (h:t) = expande h ++ converteMSet t

-- Exercício 39 
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet x [] = [(x,1)] 
insereMSet x ((y,n):t) | x == y = (y,n+1):t 
                       | otherwise = (y,n): insereMSet x t

-- Exercício 40 
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = [] 
removeMSet x ((y,n):t) | x == y && n == 1 = t 
                       | x == y = (y, n-1):t 
                       | otherwise = (y,n): removeMSet x t

-- Exercício 41 
auxConstroi :: Ord a => Int -> [a] -> [(a,Int)]
auxConstroi n [x] = [(x,n)]
auxConstroi n (x:y:t) | x == y = auxConstroi (n+1) (x:t) 
                      | otherwise = (x,n): auxConstroi 1 (y:t)

constroiMSet :: Ord a => [a] -> [(a,Int)] 
constroiMSet [] = []
constroiMSet l = auxConstroi 1 l 

-- Exercício 42

-- menos eficiente (percorre o dobro das vezes a lista)
auxLeft :: [Either a b] -> [a] 
auxLeft [] = [] 
auxLeft ((Left a):t) = a: auxLeft t 

auxRight :: [Either a b] -> [b] 
auxRight [] = []
auxRight (Right b:t) = b: auxRight t

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers l = (auxLeft l, auxRight l)

-- Exercício 43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (Just a: t) = a: catMaybes t
catMaybes (Nothing: t) = catMaybes t

-- Exercício 44
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int) 
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of 
                        Norte -> posicao (x, y+1) t 
                        Sul -> posicao (x, y-1) t 
                        Este -> posicao (x+1, y) t 
                        Oeste -> posicao (x-1, y) t

-- Exercício 45 
caminho :: (Int,Int) -> (Int,Int) -> [Movimento] 
caminho (x,y) (z,w) | x == z && y == w = [] 
                    | x > z = Oeste: caminho (x-1, y) (z,w) 
                    | x < z = Este: caminho (x+1, y) (z,w)
                    | y > w = Sul: caminho (x, y-1) (z,w)
                    | y < w = Norte: caminho (x, y+1) (z,w)

-- Exercício 46 
vertical :: [Movimento] -> Bool 
vertical [] = True 
vertical (Norte: t) = vertical t 
vertical (Sul: t) = vertical t 
vertical _ = False 

-- Exercício 47 
data Posicao = Pos Int Int deriving Show

distanciaAoQuadrado :: Posicao -> Posicao -> Int 
distanciaAoQuadrado (Pos x y) (Pos z w) = (z-x)^2 + (w-y)^2

maisCentral :: [Posicao] -> Posicao 
maisCentral [p] = p 
maisCentral (p1:p2:t) | distanciaAoQuadrado p1 centro <= distanciaAoQuadrado p2 centro = maisCentral (p1:t)
                      | otherwise = maisCentral (p2:t)
    where centro = Pos 0 0

-- Exercício 48 
naoIguais :: Posicao -> Posicao -> Bool 
naoIguais (Pos x y) (Pos z w) = x /= z || y /= w

vizinhos :: Posicao -> [Posicao] -> [Posicao] 
vizinhos p [] = [] 
vizinhos p1 (p2:t) | distanciaAoQuadrado p1 p2 <= 2 && naoIguais p1 p2 = p2: vizinhos p1 t 
                   | otherwise = vizinhos p1 t

-- Exercício 49 
mesmaOrdenada :: [Posicao] -> Bool 
mesmaOrdenada ((Pos x y):(Pos z w):t) = y == w && mesmaOrdenada((Pos x y):t)
mesmaOrdenada _ = True 

-- Exercício 50
data Semaforo = Verde | Amarelo | Vermelho deriving Show

contaNaoVermelhos :: [Semaforo] -> Int 
contaNaoVermelhos [] = 0
contaNaoVermelhos (h:t) | show h /= "Vermelho" = 1 + contaNaoVermelhos t 
                        | otherwise = contaNaoVermelhos t 

-- com case
contaNaoVermelhos' :: [Semaforo] -> Int 
contaNaoVermelhos' [] = 0
contaNaoVermelhos' (h:t) = case h of 
                            Vermelho -> contaNaoVermelhos' t 
                            _ -> 1 + contaNaoVermelhos' t

interseccaoOK :: [Semaforo] -> Bool 
interseccaoOK l = contaNaoVermelhos' l <= 1