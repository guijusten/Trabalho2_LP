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
  -- Pegando o input do user
  listInput <- get_input

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
get_input :: IO ([Int])
get_input = do
    putStr "? "
    input <- getLine
    let listInput = map read $ words input
    flag <- validate_input listInput
    if flag == 1 then get_input else return listInput


-- Função para validar o input do usuário
validate_input listInput = do
  if length listInput /= 4 then (putStrLn "A senha inserida não contém quatro dígitos. \n" >> return 1) else return 0
  if length (filter (<1) listInput) > 0 then (putStrLn "A senha inserida contém valores fora do intervalo [1, 6]. \n" >> return 1) else return 0
  if length (filter (>6) listInput) > 0 then (putStrLn "A senha inserida contém valores fora do intervalo [1, 6]. \n" >> return 1) else return 0


-- Função para auxiliar no cálculo dos acertos parciais
get_partial_hits :: [Int] -> [Int] -> IO Int
get_partial_hits password input = do
  let aux1 = remove_elt (input !! 0) password
  let aux2 = remove_elt (input !! 1) aux1
  let aux3 = remove_elt (input !! 2) aux2
  let aux4 = remove_elt (input !! 3) aux3
  return (length aux4)


-- Função para gerar a senha aleatoriamente
generate_password :: IO [Int]
generate_password = replicateM 4 $ randomRIO (1,6)


-- Função que remove um dado elemento de uma lista
remove_elt _ [] = []
remove_elt num (x:xs)  | num == x      = xs
                       | otherwise = x : remove_elt num xs
