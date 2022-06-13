-- Guilherme Fiorini Justen - 201965041AC

-- Imports
import System.Random

-- password = [x | x <- randomIO(1, 6)]

-- Função que gera o código de quatro dígitos
generate_password = sequence $ replicate 4 $ randomRIO (1,6::Int)

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
    password <- generate_password
    input <- getInput
    -- validInput <- validate_input (input)
    -- act_on_input
    putStrLn input
    putStrLn (show password)



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