import System.Random


randomInt :: (Int, Int) -> IO Int
randomInt range = randomRIO range :: IO Int

choseRandom :: [Int] -> IO Int
choseRandom list = 
    randomInt(0, (length list) - 1) >>= (\num -> return (list !! num))


get_list = sequence $ replicate 4 $ choseRandom([1,2,3,4,5,6])

get_num (x:xs) = x

getInput :: IO (String)
getInput = do
    putStr "? "
    input <- getLine
    return input


first_elt (x:xs) = x
second_elt (x:x2:xs) = x2
third_elt (x:x2:x3:xs) = x3
forth_elt (x:x2:x3:x4:xs) = x4

show_pass input = (show (first_elt input) ++ " " ++ show (second_elt input) ++ " " ++ show (third_elt input) ++ " " ++ show (forth_elt input))



loop :: IO ()
loop = do
    input <- getInput
    
    -- listInput é o input do usuário, só que no formato [Int]
    let listInput = map read $ words input :: [Int]
    let listPassword = enlist_password
    if ((first_elt (listInput) /= first_elt (listPassword)) || (second_elt (listInput) /= second_elt (listPassword)) || (third_elt (listInput) /= third_elt (listPassword)) || (forth_elt (listInput) /= forth_elt (listPassword))) then loop else return ()


enlist_password = map read $ words (show_pass ([1,2,3,4])) :: [Int]



{-
my_index :: [Int] -> (IO Int) -> Int
my_index [] num = -1
my_index (x:xs) 0 = x
my_index (x:xs) num = my_index xs (num - 1)

gennum = my_index [1,2,3,4,5,6] (randomRIO (1,6::Int))











-}