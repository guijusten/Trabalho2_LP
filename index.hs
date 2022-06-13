-- Guilherme Fiorini Justen - 201965041AC

-- Imports
import System.Random

-- randomIO(1, 6)

getInput :: IO ()
getInput = do
    putStr "? "
    input <- getLine
    putStrLn input