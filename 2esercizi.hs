-- MAncano 7 - 8 - 11


import Data.Char
--Esercizio 1
ord1 :: (Ord a) => [a] -> Bool
ord1 (x:xs) | (null xs) = True
            | otherwise = x <= head(xs) && (ord1 xs)

--Esercizio 2B
sottolisteL :: [a] -> [[a]]
sottolisteL [] = []
sottolisteL (x:xs) =  xs : sottolisteL xs

--Esercizio 2A
sottolisteR [] = []
sottolisteR xs = l : sottolisteR l
                where
                    l = (reverse (drop 1 (reverse xs)))


--Esercizio 3 
subString :: (Eq a, Num b) => [a] -> [a] -> b 
subString s l = subString2 s l 0

subString2 :: (Eq a, Num b) => [a] -> [a] -> b -> b
subString2 s l n | (null s) = 0
                 | (null l)  = -1
                 | otherwise = if (checka s l) then n else (subString2 s (tail l) (n+1))

checka :: Eq a => [a]->[a]->Bool
checka []  _ = True
checka _  [] = False
checka (s:ss) (l:ls) = (s == l) && (checka ss ls) 

--Esercizio 4
parole :: [Char] -> [[Char]]
parole w  | (null w) = []
          | (isSpace (head w)) = parole (dropWhile isSpace w) 
          | otherwise = (takeWhile isLetter w) : parole (dropWhile isLetter w)

--Esercizio 5
uniocc :: (Eq a) => [a] -> [a]
uniocc [] = []
uniocc xs = depu xs (doppioni xs)

doppioni :: (Eq a) => [a] -> [a]
doppioni [] = []
doppioni (x:xs) = if (x `elem` xs) then x : doppioni xs else doppioni xs

depu :: (Eq a) => [a] -> [a] -> [a]
depu (x:xs) dopp | (null xs) = []
                 | (null dopp) = (x:xs)
                 | otherwise = if (x `notElem` dopp) then x : (depu xs dopp) else (depu xs dopp)

                          

--Moltiplicazione tra Matrici (Esercizio 6)
--Controllo che ogni riga sia della stessa lunghezza
samelength ::  (Num a) => [[a]] -> Bool
samelength  [[]] = True
samelength xss =  foldr (&&) True [length xs == length (head xss) | xs <- xss]

-- Controllo che il numero di righe corrisponda al numero di colonne
-- Assumo chiamare samelength prima di cxl, quindi mi basta controllare solo la lunghezza di una riga e non tutte
cxl ::  (Num a) => [[a]] -> [[a]] -> Bool
cxl  [[]] [[]] = True
cxl xss yss =  (length (head xss) == length yss)

--versione alternativa con i controlli completi 
cxl2 ::  (Num a) => [[a]] -> [[a]] -> Bool
cxl2 xss yss =  foldr (&&) True [length yss == length xs | xs <- xss]

--Moltiplicazione tra matrici
mulmatrix :: (Num a) => [[a]] -> [[a]] -> [[a]]
mulmatrix xss yss = if (check_matrix xss yss) then
            [ [ foldr (+) 0 
                                    [(xs !! a) * ((yss !! a)!! b) | a <- range_yss  ] 
                                        | b <- range_xss ] 
                                        | xs <- xss]
                    else [[]]-- in caso di input scorretto
                          where
                                range_xss = [0..(length xss)-1]
                                range_yss = [0..(length yss)-1]

check_matrix :: Num a => [[a]] -> [[a]] -> Bool
check_matrix [[]] _ = False
check_matrix _ [[]] = False
check_matrix xss yss =  if (samelength xss) then
              if (samelength yss) then
                  if (cxl xss yss) then True
                    else False
                else False
              else False

        

--Esercizio 7 Laplace

determinante :: (Num a) => [[a]] -> a
determinante xss | (null xss) = 0
                 | (length xss) == 2 = ( (xss!!0)!!0 * (xss!!1)!!1 ) - ( (xss!!0)!!1 * (xss!!1)!!0 )
                 | otherwise = sum [ ((head (xss !! k)) * (if (even k) then 1 else -1)) *  determinante(rip xss k) | k <- [0..(length xss)-1] ]


rip :: (Num a) => [[a]] -> Int -> [[a]]
rip xss n = [ drop 1 (xss !! k) | k <- [0..(length xss)-1], k/=n]


--Esercizio 9 Modifica
modifica :: (Num t1) => [(t2, t1)] -> t -> [(t, t1)]
modifica xs c = [(c, ((\(y,z)->z) cp) + 1 ) | cp <- xs ]
            
--Esercizio 10
{-
comando: \xs -> zip (map (\(y,z) -> y) xs) (map (\(y,z) -> z) xs)

Analis:
1. (\(y,z) -> y) --> prende il primo esemento di una coppia
2. (map (\(y,z) -> y) xs) -> prende il primo elemento di ogni coppia della lista di coppie xs
3. zip (map (\(y,z) -> y) xs) (map (\(y,z) -> z) xs) --> combina gli elemnti estratti ricreando la lista xs originale

4. esempio di xs: [(1,2),(3,4),(5,6)]

5. (\xs -> zip (map (\(y,z) -> y) xs) (map (\(y,z) -> z) xs)) [(1,2),(3,4),(5,6)]

-}




--applyMS (Esercizio 12b) (esempio min max)
applyMS :: [a1 -> a2 -> a] -> [a1] -> [a2] -> [a]
applyMS fs xs ys = [ (f x y) |x <- xs, f <- fs, y <- ys]

--applyM (Esercizio 12a) (come il 12b ma ho cambiato i tipi dei domini)
applyM :: [a1 -> b -> a] -> [a1] -> [b] -> [a]
applyM fs xs ys = [ (f x y) |x <- xs, f <- fs, y <- ys]

--applyF (Esercizio 13)
applyF :: [a -> b] -> [a] -> [b]
applyF xs ys = [ y | x <- xs, y <- (map x ys)]

--composeM (Esercizio 14)
composeM :: [t -> t2 -> t3] -> [t1 -> t] -> [t4 -> t2] -> [t1 -> t4 -> t3]
composeM bs us vs =  [ \x y -> (bs!!k) ((us!!k)x) ((vs!!k)y) | k<-[0..(length bs)-1 ]  ] 
--Testing: head (composeM [(min)] [(+1)] [(+2)]) 2 5

--ComposeMS (Esercizio 15)
composeMS :: [t1 -> t1 -> t2] -> [t -> t1] -> [t -> t2]
composeMS bs us = [ \x -> b (u x) (v x) | b <- bs, u <- us, v <- us ]
 --Testing: ( (composeMS [min] [(+1),(+2),(+3)]) !! 1) 5

 -- Perser di parentesi () (NON FUNZIONANTE MA ERA BELLO E QUINDI ME LO SONO SALVATO)
{-
parser [] = True
parser (w:ws) | (w == '(') = True && (rico ws) 
            | otherwise = False
                
rico [] = False
rico (w:ws) | (w == '(') = rico ws
            | (w == ')') = True
            | otherwise = False
-}

-- Perser di parentesi (esercizio 16)
parentesi :: [Char] -> Bool
parentesi [] = True
parentesi xs = parser xs 0

parser :: (Num a, Eq a) => [Char] -> a -> Bool
parser [] 0 = True
parser [] _ = False
parser (w:ws) n | (w == '(') = parser ws (n+1)
                | (w == ')') = if (n == 0) then False else parser ws (n-1)
                | otherwise = False