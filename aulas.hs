-- Aulas já completamente entendidas
{--
  Lambda Cálculo
  Introdução à Linguagem Haskell
  Listas em Haskell
  Avaliação Preguiçosa
  Funções de Ordem Superior
  Tipos de Dados Algébricos
  Tipos Paramétricos
  Polimorfismo em Haskell
--}


-- Aulas não completamente entendidas
{--
  Functores
  Classes de Tipo Applicative
  A Classe de Tipos Monad
  Noção de Entrada e Saída em Haskell
  Mônada e a Notação Do
  Ideia de Mônada de Estado
--}



-- Exemplos de funções já implementadas em Haskell

my_concat [] ys = ys
my_concat (x:xs) ys = x:(my_concat xs ys)

my_index [] num = -1
my_index (x:xs) 0 = x
my_index (x:xs) num = my_index xs (num - 1)

my_length [] = 0
my_length (x:xs) = 1 + my_length(xs)

my_reverse [] = []
my_reverse (x:xs) = (my_reverse xs) ++ [x]

my_take 0 (x:xs) = []
my_take num [] = []
my_take num (x:xs) =  x:(my_take (num - 1) xs)

my_drop 0 xs = xs
my_drop num [] = []
my_drop num (x:xs) = my_drop (num - 1) xs

my_zip [] (y:ys) = []
my_zip (x:xs) [] = []
my_zip (x:xs) (y:ys) = (x, y):(my_zip xs ys)


-- Funções auxiliares

first_elt (x:xs) = x

last_elt (x:[]) = x
last_elt (x:xs) = last_elt xs


-- ================================================== Aula Funções de Ordem Superior ================================================== 
-- Basicamente foldr e foldl


-- ================================================== Aula Tipos de Dados Algébricos ================================================== 
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

area (Circle x y r) = pi * r * r
area (Rectangle x1 y1 x2 y2) = (x2 - x1) * (y2 - y1)

data Person = Person{
    name :: String,
    age :: Int,
    height :: Int,
    weight :: Int
}

-- Para acessar qualquer um dos campos, digitar no terminal: "propriedade" "nome_da_variavel"


-- ================================================== Aula Tipos Paramétricos ==================================================
-- Nem sei mané


-- ================================================== Aula Polimorfismo ==================================================
data Color = Red | Green | Blue

instance Eq Color where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

instance (Eq a) => Eq (Tree a) where
    (==) Empty Empty = True
    (==) (Node a esq1 dir1) (Node b esq2 dir2) = a == b && esq1 == esq2 && dir1 == dir2
    (==) _ _ = False

-- Classes em Haskell: Eq, Ord, Read e Show, Enum


-- ================================================== Aula Noção de IO ==================================================

main :: IO ()
main = do
    putStrLn "Hi, what is your name?"
    name <- getLine
    putStrLn ("Greetings " ++ name ++ "!")

main2 :: IO ()
main2 = do
    putStrLn "Hi, what is your name?"
    name <- getLine
    let tam = show (length name)
    putStrLn ("Your name has " ++ tam ++ " characters")


-- ================================================== Aula Mônadas e Notação do ==================================================

sum345 = do
    x <- Just 3
    y <- Just 4
    z <- Just 5
    return (x + y + z)