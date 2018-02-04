import Data.Char
import System.IO

--Eseritazione numero 3

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             guess word

--Prende l' input e lo nasconde sotto *
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                   putStrLn "You got it!"
                else
                   do putStrLn (diff word xs)
                      guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

--Esercizio 1

printSC :: String -> Char -> IO ()
printSC [] _ = return ()
printSC xs a =  do print [ x | x <- xs, x /= a]
                   return ()

--writeFile o appendFile (di tipo FilePath -> String -> IO ())
--readFile di tipo FilePath -> IO String 

--Esercizio 2

rF :: String -> IO String 
rF xs = readFile xs

{-
rF = do xs <- getLine
        putStrLn xs
        readFile xs
        return ()
-}

--Esercizio 3
--Versione solo prima riga
ioFile :: FilePath -> FilePath -> IO ()
ioFile src dst =  do xs <- readFile src
                     appendFile dst (head (words [ toUpper x | x <- xs ]))
                     return()

--Versione file completo
ioFileAll :: FilePath -> FilePath -> IO ()
ioFileAll src dst =  do xs <- readFile src
                        appendFile dst [ toUpper x | x <- xs ]
                        return()

--Esercizio 4
rKwF :: FilePath -> IO () 
rKwF path =  do xs <- getLine 
                appendFile path xs
                return()

--Esercizio 5
rFI :: FilePath -> IO Int
rFI src =  do xs <- readFile src
              return (read (head (words xs)) :: Int)

--Esercizio 6
factIO eof = do putStr "\nBuongiornissimo, insert namba: "
                xs <- getLine
                if xs == eof then return ()
                  else do  putStr "\nIl fattoriale e': "
                           print (factorial ( read xs :: Int) )
                           factIO eof
                           return ()

 
factorial 0 = 1
factorial n = n * factorial (n - 1)     

--Esercizio 7
creare una lista di parole con quelle presente nel file

creare la lista di parole possibili permutando le lettere in input
stampare tutte le parole della lista di angrammi che sono matchate nella lista di parole nel file (element)           
