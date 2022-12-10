import Control.Applicative (liftA2)
import Data.List (findIndex, nub, tails)

result :: Eq a => Int -> [a] -> Maybe Int
result n = ((+n) <$>) . findIndex ((==n) . length . nub . take n) . tails

main :: IO ()
main = do
    input <- readFile "day6.txt"
    putStrLn (show $ result 4 input)
    putStrLn (show $ result 14 input)
