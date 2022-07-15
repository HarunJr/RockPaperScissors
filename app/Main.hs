import System.Random
import Control.Monad

data Move = Rock | Paper | Scissors deriving (Eq, Show, Read)
data Result = Win | Lose | Tie deriving (Eq, Show)

type Strategy = [Move] -> Move

score :: Move -> Move -> Result
score Rock Scissors  = Win
score Rock Paper     = Lose
score Scissors Rock  = Lose
score Scissors Paper = Win
score Paper Rock     = Win
score Paper Scissors = Lose
score _ _            = Tie

-- Fundermental Strategies

-- /plays only Rock
alwaysRock :: Strategy 
alwaysRock _ = Rock 

-- Copy the previous move made by opponent
copyCat :: Strategy
copyCat [] = Rock
copyCat (latest:_) = latest

cycleS :: Strategy
cycleS moves = 
    case length moves `mod` 3 of
        0 -> Rock
        1 -> Paper
        2 -> Scissors

alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2 moves = 
    case (length moves) `mod` 2 of
        0 -> str1 moves
        1 -> str2 moves

switchUp :: Strategy -> Strategy
switchUp str moves = 
    case str moves of
        Rock -> Paper
        Paper -> Scissors
        Scissors -> Rock

switchDown :: Strategy -> Strategy
switchDown str = switchUp (switchUp str)

-- Complex Strategies
complexStrategy :: Strategy
complexStrategy = 
    switchUp copyCat `alternate` switchDown cycleS

aiStrategy :: Strategy -> [Move] -> String -> Int -> IO ()
aiStrategy strategy moves move rounds = do
    case move of
        "Rock" -> playGame strategy (Rock:moves) rounds
        "Paper" -> playGame strategy (Paper:moves) rounds
        "Scissors" -> playGame strategy (Scissors:moves) rounds
        _ -> return ()

-- intToMove :: Int -> Move
-- intToMove 1 = Rock
-- intToMove 2 = Paper
-- intToMove 3 = Scissors
    
playGame :: Strategy -> [Move] -> Int -> IO ()
playGame strategy moves rounds = do
    putStrLn " Enter Move: \n Rock | Paper | Scissors | Quit\n"

    input <- getLine
    let myPlay = read input :: Move

    let aIPlay = strategy moves
    putStrLn $ "AI Plays: \n" ++ show aIPlay++"\n"

    let result =  score myPlay aIPlay
    putStrLn $ "Result: " ++ show result ++"\n"

    case myPlay `score` aIPlay of
        Win  -> putStrLn "Yay You win!\n"
        Lose -> putStrLn "You lost! beter luck next time :(\n"
        Tie  -> putStrLn "It's a Tie :\ \n"
               
    when (rounds > 1) $ aiStrategy strategy moves input (rounds - 1) -- Use Control.Monad

    -- if rounds > 1
    --     then aiStrategy strategy moves input (rounds - 1)
    --     else return ()
    -- case( rounds > )1 of
    --     True -> aiStrategy strategy moves input (rounds - 1)
    --     False -> return ()


    -- case input of 
    --     "Rock" -> playGame strategy (Rock:moves)
    --     "Paper" -> playGame strategy (Paper:moves)
    --     "Scissors" -> playGame strategy (Scissors:moves)
    --     _ -> return ()

-- getRandomNumber :: IO ()
-- getRandomNumber = do
--     num <- randomRIO (0, 3) :: IO Int
--     let randomMove =  intToMove num
--     putStrLn ("The move is " ++ show randomMove)

main :: IO ()
main = do
    putStrLn " How many rounds?\n 1 | 3 | 5"
    rounds <- getLine

    playGame complexStrategy [] (read rounds)
    putStrLn "Game Over!"