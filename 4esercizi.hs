
-- Esercizio 1: la lista dei primi n quadrati
square :: (Enum a, Num a) => a -> [a]
square n =  [x^2 | x <- [0..n]]

--Esercizio 2 RepliKant
replicant :: (Enum a,Num a) => a -> b -> [b]
replicant c e = [y | x <-[1..c], y <-[e]]

-- Esercizio Perfect

 factors :: (Enum a, Integral a)=> a -> [a]
 factors n = [x | x <- [1..n-1], n `mod` x == 0]

 perfects :: (Enum n, Eq n, Num n, Integral n)=> n -> [n]
 perfects n = [x | x <- [0..n], x == sum(factors x)]

--Permutazioni
permutations :: (Eq a) => [a] -> [[a]]
permutations xs     | (length xs) == 1 = [xs]
				    | otherwise = [(x:y) | x <- xs, y <- (permutations(hints x xs))]


hints :: (Eq a) => a -> [a] -> [a]
hints a (x:xs) | (a == x) = xs
               | otherwise =  [x] ++ (hints a xs)