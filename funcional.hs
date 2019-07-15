module Main where
import Data.Array
import Text.Printf
import System.IO

---------------------- DIVISAO EXATA, INTEIRA E RESTO ----------------------
main :: IO ()
main = do
 putStrLn "digite uma palavra: "
 x <- getLine
 putStrLn "digite outra palavra: "
 y <- getLine
 putStrLn ("Divisão inteira : " ++ show (divInteira (read x) (read y)))
 putStrLn ("resto : " ++ show (resto (read x) (read y)))
 putStrLn ("Divisão Exata : " ++ show (divExata (read x) (read y)))

---------------------- BLITZ ---------------------------
blitz :: IO ()
blitz = do
 licenciamento <- getLine
 carteira <- getLine
 bafometro <- getLine
 putStrLn (verificaBlitz (read licenciamento) (read carteira) (read (bafometro) :: Double))

verificaBlitz :: Int -> Int -> Double -> String
verificaBlitz x y z | (x < 30 && y < 30 && z <= 0.05) == True = "False"
               | otherwise = "True"

---------------- ULTIMAS VOGAIS DE 5 PALAVRAS -----------------
ultimasVogais :: IO ()
ultimasVogais = do
 palavra1 <- getLine
 palavra2 <- getLine
 palavra3 <- getLine
 palavra4 <- getLine
 palavra5 <- getLine
 let palavras = [palavra1, palavra2, palavra3, palavra4, palavra5]
 putStrLn (retornaVogais palavras)

retornaVogais :: [String] -> String
retornaVogais [] = ""
retornaVogais (x:xs) | ((verifica x) == "a" || (verifica x) == "e" || (verifica x) == "i" || (verifica x) == "o"
                                  || (verifica x) == "u") = verifica x ++ retornaVogais xs
                     | otherwise = retornaVogais xs

verifica :: String -> String
verifica [x] = [x]
verifica (x:xs) = verifica xs

----------------------- ONDE ESTA WALLY -------------------------
wally :: IO()
wally = do
 putStrLn "digite nomes: "
 x <- getLine
 if (x == "wally")
   then return()
   else do
   let palavras = words x
   putStrLn ("nome possivel : " ++ pegaUltimo (words (pegaCinco palavras)))
   wally

pegaCinco :: [String] -> String
pegaCinco [] = ""
pegaCinco (x:xs) | length x == 5 =  x ++ " " ++ pegaCinco xs
                 | otherwise = pegaCinco xs

pegaUltimo :: [String] -> String
pegaUltimo [] = "?"
pegaUltimo [x] = x
pegaUltimo (x:xs) = pegaUltimo xs

-- definir funcao
fatorial 0 = 1
fatorial n = fatorial(n-1) * n

-- conceito de IF
guarda x | (x < 10) = True
         | otherwise = False

-- Variaveis Anonimas _
e :: Bool -> Bool -> Bool
e False _ = False
e _ False = False
e True True= True

-- Tuplas
somaTupla :: (String, Int) -> (String, Int) -> (String, Int)
somaTupla (a,b) (c,d) = (a++c,b+d)

-- extrair elementos Tuplas
-- fst (x,y) extrai o primeiro
-- snd (x,y) extrai o segundo
first (x,_,_) = x
second (x,y,z) = y

-- definir novos tipos
type Nome = String
type Idade = Int
type Peso = Float
type Pessoa = (Nome, Idade, Peso) -- tipo composto

lucas :: Pessoa -- definindo variavel
lucas = ("lucas", 19, 78) -- variavel

getIdade :: Pessoa -> Idade
getIdade (x,y,z) = y

-- listas
lista = [1,2..10]
-- adicionar elemento no inicio
-- elemento:lista
-- 1:2:3:[] construir lista

minimo :: Int -> Int -> Int
minimo x y | x > y = y
           | otherwise = x

------------------- FUNCOES -----------------------
replace :: Int -> [a] -> [a] -> [a]
replace _ [] _ = []
replace _ xs [] = xs
replace 0 (_:xs) ys = ys ++ xs
replace n (x:xs) ys | n == 0 = ys ++ xs
                    | otherwise = [x] ++ replace (n-1) xs ys

maiorPalavra :: String -> String -> String
maiorPalavra a b | (length a) > (length b) = a
                 | otherwise = b

contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x:xs) element | x == element = True
                        | otherwise = contains xs element

compareList :: [Int] -> [Int] -> Bool
compareList [] [] = True
compareList [] _ = False
compareList _ []= False
compareList (x:xs) (y:ys) | x == y = compareList xs ys
                          | x /= y = False

reverseList :: [t] -> [t]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

maiorList :: [Int] -> Int
maiorList [] = 0
maiorList (x:xs) | x > maiorList xs = x
                 | otherwise = maiorList xs

verPar :: [Int] -> Bool
verPar [] = True
verPar (x:xs) | mod x 2 == 0 = verPar xs
              | otherwise = False

somaElements :: [Int] -> Int
somaElements [] = 0
somaElements (x:xs) = x + somaElements xs

uniao :: [Int] -> [Int] -> IO()
uniao lista1 lista2 = do
  if (length lista2 == 0) then do
    print lista1
  else do
    let element = head lista2
    if (contains lista1 element) then do
      uniao lista1 (tail lista2)
    else do
      uniao (lista1 ++ [element]) (tail lista2)

-- [x : x <- [1,2..10], cond,cond...]
-- zip [1,2,3] [4,5,6] = [(1,4),(2,5),(3,6)]

-- if then else
if_par :: Int -> Bool
if_par n = if (mod n 2 == 0) then True else False

-- case
case_par :: Int -> Bool
case_par n = case (mod n 2 == 0) of
 True -> True
 False -> False

-- where
quadrado :: (Float) -> (Float)
quadrado n = quadn where quadn = (n)*(n)

-- split
--words "alguma coisa" = ["alguma", "coisa"]

getArray = array ((1,1), (2,2)) [((1,1),'a'),((1,2),'b'),((2,2),'c'),((2,1),'d')]

-- Objetos
data People = Programador Nome Idade | Aluno Nome Idade -- varios construtores
                  deriving(Show)
aluno1 = Aluno "lucas" 19
aluno2 = Aluno "anthony" 19

isProgrammer :: People -> Bool
isProgrammer (Programador _ _) = True
isProgrammer (Aluno _ _ )= False

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
          | otherwise = True

trocaA :: String -> String
trocaA [] = []
trocaA (x:xs) | x == 'a' = ['b'] ++ trocaA xs
              | otherwise = [x] ++ trocaA xs


divInteira :: Int -> Int -> Int
divInteira x y = div x y

resto :: Int -> Int -> Int
resto x y = mod x y

divExata :: Float -> Float -> Float
divExata x y = x/y

max :: Int -> Int -> Int -> Int
max a b c | (a > b && b > c) = a
          | b > c = b
          | otherwise = c
          
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim

--------------- EXEMPLO REDIN -----------------
removeElem :: [Int] -> Int -> [Int]
removeElem [] _ = []
removeElem (x:xs) i | i == x = removeElem xs i
                    | otherwise = [x] ++ removeElem xs i

esvaziaLista :: [Int] -> IO()
esvaziaLista lista = do -- começa com a lista full [1,2,3,4,5]
  if (length lista == 0) then do
    putStrLn ("Parabens, voce esvaziou a lista!")
  else do
    putStrLn ("Que numero quer remover?\n")
    print lista
    input <- getLine -- pede pra remover o 3
    let op = read input
    esvaziaLista (removeElem lista op) -- chama recursivamente passando a lista atualizada [1,2,4,5]

        ----------- BHASKARA ------------
raizes :: Float -> Float -> Float -> String
raizes a b c | delta a b c >= 0 = ("x1 = " ++ show (x1 a b c) ++ " x2 = " ++ show (x2 a b c))
             | otherwise = "Nao possui raiz real"

x1 :: Float -> Float -> Float -> Float
x1 a b c = (-(b) + sqrt(delta a b c))/(2*(a))

x2 :: Float -> Float -> Float -> Float
x2 a b c = (-(b) - sqrt(delta a b c))/(2*(a))

delta :: Float -> Float -> Float -> Float
delta a b c = ((quadrado b) - (4*a*c))

-- filter
-- filter (Bool) [lista]
-- filter (isPrime) [1 .. 10]

abcde :: [Int]
abcde = [1,2,3,4]

getabcde :: [Int]
getabcde = abcde

removeabcde :: [Int] -> [Int]
removeabcde (x:xs) = xs

-----------------

{-type Seed = Int
random :: Seed -> (Int, Seed)
random s = (mersenneTwisterPerturb s, splitSeed s)-}

shuffle :: [Int] -> [a] -> [a]
shuffle _ [] = []
shuffle (i:is) xs = let (firsts, rest) = splitAt (i `mod` ((length xs)+2)) xs
                     in (last firsts) : shuffle is (init firsts ++ rest)

insertByIndex :: [a] -> a -> Int -> [a]
insertByIndex [] _ _ = []
insertByIndex (x:xs) element i | i == 0 = [element] ++ insertByIndex xs element (i-1)
                               | otherwise = [x] ++ insertByIndex xs element (i-1)

---------------------------------------------------------------------------------------------------
-------------------------------------- QUESTÕES URI -----------------------------------------------
---------------------------------------------------------------------------------------------------

   ------ TCC da Depressão Natalino --------

-- import System.IO
tcc :: IO ()
tcc = do
 x <- getLine
 let list = (split x ' ')
 let y = head list
 let z = last list
 let a = read y :: Int
 let b = read z :: Int
 putStrLn (verificaTcc a b)

verificaTcc :: Int -> Int -> String
verificaTcc entrega prazo | entrega > prazo || prazo > 24 = "Eu odeio a professora!"
                     | prazo - entrega >= 3 = "Muito bem! Apresenta antes do Natal!"
                     | prazo - entrega <= 3 && entrega+2 >= 24 = "Parece o trabalho do meu filho!\nFail! Entao eh nataaaaal!"
                     | otherwise = "Parece o trabalho do meu filho!\nTCC Apresentado!"

----------- Léxico -------------

-- import System.IO

lexico :: IO ()
lexico = do
 word1 <- getLine
 word2 <- getLine
 let lista1 = split word1 ' '
 let lista2 = split word2 ' '
 putStrLn (verificaLexico word1 word2 lista1 lista2)

verificaLexico :: String -> String -> [String] -> [String] -> String
verificaLexico word1 word2 (x:xs) (y:ys) | x == "" || y == "" = verificaLexico word1 word2 xs ys
                                   | x == y && (length xs) == 0 && (length ys) > 0 = word1 ++ "\n" ++ word2
                                   | x == y && (length ys) == 0 && (length xs) > 0 = word2 ++ "\n" ++ word1
                                   | x == y && (length xs) == 0 && (length ys) == 0 = word1 ++ "\n" ++ word2
                                   | x == y = verificaLexico word1 word2 xs ys
                                   | x < y = word1 ++ "\n" ++ word2
                                   | x > y = word2 ++ "\n" ++ word1
                                   | otherwise = "aaa"
