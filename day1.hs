import Helpers
import Data.List
import Text.Read

readCalories :: String -> [Maybe Int]
readCalories = reverse . sort . map (sumMaybe . map readMaybe) . split "" . lines

main :: IO()
main = do
    input <- readFile "day1.txt"
    let calories = readCalories input
    putStrLn (show $ head calories)
    putStrLn (show $ (sumMaybe . take 3) calories)
