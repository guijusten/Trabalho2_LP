-- Guilherme Fiorini Justen - 201965041AC

-- Imports
import System.Random

-- password = [x | x <- randomIO(1, 6)]

-- Função que gera o código de quatro dígitos
-- generate_password = sequence $ replicate 4 $ randomRIO (1,6::Int) >>= print

generate_password = do
  g <- getStdGen
  print $ take 10 (randoms g :: [Int])

data Password = Password {
  v1 :: Int,
  v2 :: Int,
  v3 :: Int,
  v4 :: Int
} deriving Show

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
    auxPassword <- generate_password
    
    -- let p = Password (randomRIO (1,6::Int)) (randomRIO (1,6::Int)) (randomRIO (1,6::Int)) (randomRIO (1,6::Int))
    let p = Password 1 2 3 4
    input <- getInput


    -- validInput <- validate_input (input)
    -- act_on_input

    putStrLn input
    let v11 = v1 p
    putStrLn (show v11)



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