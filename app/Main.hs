import System.Random
import Control.Monad
import Control.Monad.State
import Data.Char

data Move = Rock | Paper | Scissors deriving (Eq, Show, Read)
data Result = Win | Lose | Tie deriving (Eq, Show)

type Point = (Int, Int)

score :: Move -> Move -> Result
score Rock Scissors  = Win
score Rock Paper     = Lose
score Scissors Rock  = Lose
score Scissors Paper = Win
score Paper Rock     = Win
score Paper Scissors = Lose
score _ _            = Tie

capWord :: String -> String
capWord "" = ""
capWord (x:xs) = toUpper x : map toLower xs

intToMove :: Int -> Move
intToMove 1 = Rock
intToMove 2 = Paper
intToMove 3 = Scissors

showResult :: Result -> IO ()
showResult result = do
    case result of
        Win  -> putStrLn "You WON this round!"
        Lose -> putStrLn "You LOST this round! :("
        Tie  -> putStrLn "It's a TIE for this round"

finalResults :: Int -> Int -> Int -> IO ()
finalResults cPoint uPoint rounds= do
    if uPoint > cPoint
        then putStrLn "YOU WON THE MATCH"
    else if uPoint < cPoint
        then putStrLn "YOU LOST THE MATCH! :("
    else playGame rounds (cPoint, uPoint)


evalPoints :: Result -> State Point ()
evalPoints r = do
    (x, y) <- get
    case r of
        Win -> put (x, y + 1)
        Lose -> put (x + 1, y)
        Tie -> put (x, y)

playGame :: Int -> Point -> IO ()
playGame rounds point= do
    putStrLn " Play a move: Rock | Paper | Scissors OR Quit"
    putStrLn "--------------------------------------------------\n"

    input <- getLine
    let myPlay = read (capWord input) :: Move -- match Move text format using capWord
    putStrLn ("You played: "++show myPlay++"\n")

    num <- randomRIO(1, 3) :: IO Int
    let compMove = intToMove num
    putStrLn ("The Comp plays: " ++ show compMove ++"\n \n")

    let result =  score myPlay compMove
    -- showResult result

    let ((), gamePoints) = runState (evalPoints result) point
    let compPoints = fst gamePoints
    let userPoints = snd gamePoints

    putStrLn "--------------------------------------------------"
    showResult result
    putStrLn "--------------------------------------------------\n"

    putStrLn ("You have: "++ show userPoints++" points and the computer has: "++ show compPoints ++" points\n \n")

    if rounds > 1 || result == Tie
        then  playGame (rounds - 1) gamePoints
    else do
            putStrLn "--------------------------------------------------"
            finalResults compPoints userPoints rounds
            putStrLn "--------------------------------------------------\n"
            return ()

    -- when (rounds > 1 || result == Tie) $ playGame (rounds - 1) gamePoints -- Use Control.Monad

main :: IO ()
main = do
    putStrLn " How many rounds?  1 | 3 | 5\n"
    rounds <- getLine
    putStrLn (show (read rounds :: Int)++" rounds\n")
    playGame (read rounds) (0, 0)
    putStrLn "Game Over!"