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

