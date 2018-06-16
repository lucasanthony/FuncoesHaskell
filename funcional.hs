module Main where
import Data.Array
import Text.Printf

main :: IO ()
main = do
 putStrLn "digite uma palavra: "
 x <- getLine
 putStrLn "digite outra palavra: "
 y <- getLine
 putStrLn ("Divisão inteira : " ++ show (divInteira (read x) (read y)))
 putStrLn ("resto : " ++ show (resto (read x) (read y)))
 putStrLn ("Divisão Exata : " ++ show (divExata (read x) (read y)))

minimo :: Int -> Int -> Int
minimo x y | x > y = y
           | otherwise = x

maiorPalavra :: String -> String -> String
maiorPalavra a b | (length a) > (length b) = a
                 | otherwise = b

wally = do
 putStrLn "digite nomes: "
 x <- getLine
 if (x == "wally")
   then return()
   else do
   let palavras = words x
   putStrLn ("Palavras : " ++ pegaCinco palavras)
   wally

pegaCinco :: [String] -> String
pegaCinco [] = ""
pegaCinco (x:xs) | length x == 5 =  x ++ " " ++ pegaCinco xs
                 | otherwise = pegaCinco xs

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


contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x:xs) element | x == element = True
                        | x /= element = contains xs element

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

-----------
type Dig = Int
type Id = Int
type Cor = String
data Carta = Carta Dig Cor Id
                 deriving(Show)
type Deck = [Carta]
data Player = Player Nome Deck
                 deriving(Show)

player1 = Player "lucas" [Carta 2 "red" 1, Carta 4 "red" 2, Carta 2 "green" 3]
player2 = Player "cout" [Carta 2 "blue" 1, Carta 9 "red" 2]
player3 = Player "tib" [Carta 5 "yellow" 1]
player4 = Player "pedo" []

players :: [Player]
players = [player1, player2, player3, player4]

showDeck :: Player -> [Carta]
showDeck (Player _ d) = d

getPlayers :: [Player] -> [Nome]
getPlayers [] = []
getPlayers ((Player nome _):xs) = nome : getPlayers xs

-----------------
