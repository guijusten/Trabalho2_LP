import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.Maybe
import Data.List

-- Função para pegar o input do usuário
getInput :: IO (String)
getInput = do
    putStr "? "
    input <- getLine
    return input


-- Funções para pegar valores individuais da senha
first_elt (x:xs) = x
second_elt (x:x2:xs) = x2
third_elt (x:x2:x3:xs) = x3
forth_elt (x:x2:x3:x4:xs) = x4


-- Loop principal da aplicação
main :: IO ()
main = do
    putStrLn " "

    -- Gerando senha aleatória
    listPassword <- generatePassword
    
    -- Chamando função loop para iniciar o jogo
    counter <- loop listPassword 1

    -- Quando o user vencer, ele sai de loop e é informado que venceu
    putStrLn " "
    putStrLn "Parabéns seu merda!!!!!!!"
    putStrLn ("Você acertou em " ++ show counter ++ " rodadas.")

    -- Checando se o user quer jogar de novo
    putStrLn "Vai jogar de novo, corno?"
    deNovo <- getLine
    if deNovo == "claro" then main else return ()


loop :: [Int] -> Int -> IO (Int)
loop listPassword counter = do
  -- Pegando o input do user e transformando-o em [Int]
  input <- getInput
  let listInput = map read $ words input :: [Int]
  print listInput
  print listPassword

  -- Calculando quantos acertos parciais e totais foram feitos
  let full_hits = length (filter (True==) (zipWith (==) listPassword listInput))
  --let parcial_hits = (length [x | x <- listInput, elem x listPassword]) - full_hits
  partial_hits_aux <- get_partial_hits listPassword listInput
  let partial_hits =  4 - partial_hits_aux - full_hits
  -- Printando informações ao user
  putStrLn (" ")
  putStrLn ("Partial Hits: " ++ show partial_hits)
  putStrLn ("Full Hits: " ++ show full_hits)

  -- Verificando se o user acertou
  if full_hits /= 4 then (loop listPassword (counter + 1)) else return counter

{--
[x | x <- [1,2,1,2], elem x [1,2,3,4]]

[1,2,1,2]
--}

get_partial_hits :: [Int] -> [Int] -> IO Int
get_partial_hits password input = do
  let pass1 = remove_elt (first_elt input) password
  let pass2 = remove_elt (second_elt input) pass1
  let pass3 = remove_elt (third_elt input) pass2
  let pass4 = remove_elt (forth_elt input) pass3
  putStrLn $ show pass4
  return (length pass4)


-- Função para gerar a senha aleatoriamente
generatePassword :: IO [Int]
generatePassword = replicateM 4 $ randomRIO (1,6)


-- Função que remove um dado elemento de uma lista
remove_elt _ [] = []
remove_elt num (x:xs)  | num == x      = xs
                       | otherwise = x : remove_elt num xs

-- fromJust $ elemIndex 3 [1,2,3,4]
{--
let passZipped = zipWith (*) listPassword [1, 7, 13, 19]
  let inputZipped = zipWith (*) listInput [1,7,13,19]
  print passZipped
  print inputZipped
--}