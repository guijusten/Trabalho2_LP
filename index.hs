-- Guilherme Fiorini Justen
-- 201965041AC

import Control.Monad (replicateM)
import System.Random (randomRIO)


-- Loop principal da aplicação
main :: IO ()
main = do
    putStrLn " "

    -- Gerando senha aleatória
    listPassword <- generate_password
    
    -- Chamando função loop para iniciar o jogo
    counter <- game listPassword 1

    -- Quando o user vencer, ele sai de loop e é informado que venceu
    putStrLn " "
    putStrLn ("Parabéns! Você acertou após " ++ show counter ++ " tentativas.")

    -- Checando se o user quer jogar de novo
    putStrLn "Para jogar de novo digite [1]. Caso contrário digite [2]"
    deNovo <- getLine
    if deNovo == "1" then main else return ()


game :: [Int] -> Int -> IO (Int)
game listPassword counter = do
  -- Pegando o input do user e transformando-o em [Int]
  input <- get_input
  let listInput = map read $ words input :: [Int]

  -- Calculando quantos acertos parciais e totais foram feitos
  let fullHits = length (filter (True==) (zipWith (==) listPassword listInput))

  partialHitsAux <- get_partial_hits listPassword listInput
  let partialHits =  4 - partialHitsAux - fullHits

  -- Printando informações ao user
  putStrLn ("Acertos parciais: " ++ show partialHits)
  putStrLn ("Acertos completos: " ++ show fullHits)
  putStrLn (" ")

  -- Verificando se o user acertou
  if fullHits /= 4 then (game listPassword (counter + 1)) else return counter


-- Função para pegar o input do usuário
get_input :: IO (String)
get_input = do
    putStr "? "
    input <- getLine
    return input


-- Funções para pegar valores individuais da senha
first_elt (x:xs) = x
second_elt (x:x2:xs) = x2
third_elt (x:x2:x3:xs) = x3
forth_elt (x:x2:x3:x4:xs) = x4


get_partial_hits :: [Int] -> [Int] -> IO Int
get_partial_hits password input = do
  let aux1 = remove_elt (first_elt input) password
  let aux2 = remove_elt (second_elt input) aux1
  let aux3 = remove_elt (third_elt input) aux2
  let aux4 = remove_elt (forth_elt input) aux3
  return (length aux4)


-- Função para gerar a senha aleatoriamente
generate_password :: IO [Int]
generate_password = replicateM 4 $ randomRIO (1,6)


-- Função que remove um dado elemento de uma lista
remove_elt _ [] = []
remove_elt num (x:xs)  | num == x      = xs
                       | otherwise = x : remove_elt num xs