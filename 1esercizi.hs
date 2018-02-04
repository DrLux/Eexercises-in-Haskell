--Esercizio 1 (safetail)

--a conditional expression --> (if then else)
safeTail :: [a] -> [a]
safeTail xs = if (length xs) == 0 then [] else (tail xs)

--guarded equations --> (switch)
safeTail2 :: [a] -> [a]
safeTail2 xs | (null xs) == True = []
             | otherwise = (tail xs)
      
-- pattern matching --> (Caso per caso)
safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 (x:xs) = xs


--  Esercizio 2 (alternative OR pattern matching)

(##) :: Bool -> Bool -> Bool
True ## True = True
True ## False = True
False ## True = True
False ## False = False


(#) :: Bool -> Bool -> Bool
False # False = False
_ # _ = True

(###) :: Bool -> Bool -> Bool
True ### _ = True
_ ### _ = False

-- Esercizio 3 (Redefinition of && with conditional)
and1 :: Bool -> Bool -> Bool
and1 a b = if a == b && a == True then True else False 

-- Esercizio 4 (Redefinition of && with conditional)
and2 :: Bool -> Bool -> Bool
and2 a b = if a == True then b else False

-- Esercizio 5 (Spezzare a meta' le liste pari)
halve :: [a] -> ([a],[a])
halve xs | even(length xs)  == True = splitlist (div (length xs) 2) xs

splitlist :: Int -> [a] -> ([a],[a])
splitlist x xs = ( (take x xs), (drop x xs))

-- Approfondimento esercizio 5
halve2 :: [a] -> ([a],[a])
halve2 xs | even(y) = splitlist (div y 2) xs
     where 
      y = length xs

-- Esercizio 6
mul1 :: Num a => a -> a -> a -> a
mul1 = \x y z -> x*y*z

-- Esercizio di Euclide
euclide :: (Num a, Ord a, Eq a) => a -> a -> a
euclide m n | m == 0 = n
            | m < n = euclide n m
            | otherwise = euclide (m-n) n  
          
-- Elevamento a potenza
pwd :: (Num a, Integral n, Eq n, Ord n) => a -> n -> a
pwd a n | n == 0 = 1
        | n > 0 && (even n) = y * y
        | otherwise = a * y * y
        where
          y = (pwd a (div n 2))

-- Fibonacci a 3 parametri
fib :: ( Eq a, Num a) => a -> a -> a -> a
fib m n k  | k == 0 = m
           | k == 1 = n
           | otherwise = fib n (m+n) (k-1)  