import Control.Monad
import Control.Monad.Random

data RPS = Rock | Paper | Scissors
         deriving (Show, Eq, Read)

data Result = Win | Lose | Tie
            deriving (Show, Eq)


eval :: RPS -> RPS -> Result
eval Rock Scissors  = Win
eval Rock Paper     = Lose
eval Scissors Rock  = Lose
eval Scissors Paper = Win
eval Paper Rock     = Win
eval Paper Scissors = Lose
eval _ _            = Tie

randRPS :: RandomGen g => Rand g RPS
randRPS = do
  v <- getRandomR (1 :: Int, 3)
  return $ case v of
    1 -> Rock
    2 -> Paper
    3 -> Scissors

randResult :: RandomGen g => Rand g Result
randResult = do
  r1 <- randRPS
  r2 <- randRPS
  return $ eval r1 r2

readLine :: Read a => IO a
readLine = liftM read getLine

randResults :: RandomGen g => Int -> Rand g [Result]
randResults i = replicateM i randResult

results :: RandomGen g => Rand g [Result]
results = sequence $ repeat randResult

simulate :: Int -> IO ()
simulate n = do
  ten <- evalRandIO $ take n `liftM` results
  let count t = length $ filter (==t) ten
  let sur s = " (" ++ s ++ ")"
  let perc c = show ((fromIntegral c / fromIntegral n) * 100) ++ "%"
  let winCount  = count Win
  let loseCount = count Lose
  let tieCount  = count Tie
  putStrLn $ "Wins: "   ++ show winCount  ++ sur (perc winCount)
  putStrLn $ "Losses: " ++ show loseCount ++ sur (perc loseCount)
  putStrLn $ "Ties: "   ++ show tieCount  ++ sur (perc tieCount)

main :: IO ()
main = simulate 100000

play :: IO ()
play = do
  myRoll  <- readLine
  cpuRoll <- evalRandIO randRPS
  putStrLn $ show myRoll ++ " vs " ++ show cpuRoll
  case myRoll `eval` cpuRoll of
    Win  -> putStrLn "Yay we win!"
    Lose -> putStrLn "I suck and lost :("
    Tie  -> play