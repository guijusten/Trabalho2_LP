-- Guilherme Fiorini Justen - 201965041AC


data Password = Password {
  v1 :: Int,
  v2 :: Int,
  v3 :: Int,
  v4 :: Int
} deriving Show

-- Função para imprimir uma senha
getPassword p = (show (v1 p) ++ " " ++ show (v2 p) ++ " " ++ show (v3 p) ++ " " ++ show (v4 p))

getInput :: IO (String)
getInput = do
    putStr "? "
    input <- getLine
    return input


-- validate_input :: (Bool)
validate_input input
  | length input /= 4 = False
  | otherwise = True


main :: IO ()
main = do
    let p = Password 1 2 3 4
    putStrLn " "
    loop
    putStrLn " "
    putStrLn "Parabéns seu merda!!!!!!!"
    putStrLn "Vai jogar de novo, corno?"
    deNovo <- getLine
    if deNovo == "claro" then main else return ()


loop = do
    let p = Password 1 2 3 4
    let parcial_hits = 0
    let full_hits = 0
    input <- getInput
    
    -- listInput é o input do usuário, só que no formato [Int]
    let listInput = map read $ words input :: [Int]

    putStrLn ("The input is: " ++ show listInput)
    let v11 = v1 p
    putStrLn ("v1 is: " ++ show v11)
    if input /= (getPassword p) then loop else return ()



{-- 
Pseudo-Code:

main = do
    Gerar Senha

    while não acertou:
        Pegar Input
        If Input Válido:
            Comparar Input com Senha
            Mostrar Resultado para o User
        Otherwise:
            Indicar que Input não é Válido
        
--}

{--
Problemas:

Validate_Input:
  O input deve ser do tipo List. Acho que se for de outro tipo o código quebra
  Não consigo retornar o Bool dessa função, para que a função possa ser chamada na main 
--}

{--
Ideia:
  Poderia criar uma estrutura de dados: data Password = Password Int Int Int Int.
  Dessa forma, definir funções em cima dessa classe para fazer validações do input, e comparações do input com a senha gerada randomicamente.  
--}

{-
Considering Random Password

import System.Random

let p = Password (randomRIO (1,6::Int)) (randomRIO (1,6::Int)) (randomRIO (1,6::Int)) (randomRIO (1,6::Int))

-- Função que gera o código de quatro dígitos
generate_password = sequence $ replicate 4 $ randomRIO (1,6::Int) >>= print

generate_password = do
  g <- getStdGen
  print $ take 10 (randoms g :: [Int])

-}