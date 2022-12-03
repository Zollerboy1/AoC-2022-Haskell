import Helpers (split)
import Data.List (sortBy)
import Data.Function (on)


readCalories :: String -> [Int]
readCalories = sortBy (compare `on` negate) . map (sum . map read) . split "" . lines

main :: IO ()
main = do
    input <- readFile "day1.txt"
    let calories = readCalories input
    putStrLn (show $ head calories)
    putStrLn (show $ (sum . take 3) calories)
