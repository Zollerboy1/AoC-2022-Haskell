-- import Control.Applicative (liftA2)
-- import Data.List (findIndex, nub, tails)

-- result :: Eq a => Int -> [a] -> Maybe Int
-- result n = ((+n) <$>) . findIndex ((==n) . length . nub . take n) . tails

-- main :: IO ()
-- main = do
--     input <- readFile "day6.txt"
--     putStrLn (show $ result 4 input)
--     putStrLn (show $ result 14 input)

import Data.List

-- Find the first index where there are four consecutive unique characters
findFirstUnique :: String -> Maybe Int
findFirstUnique xs = findIndex (\ys -> length (nub (take 4 ys)) == 4) (tails xs)

-- Print the result
main :: IO ()
main = do
    input <- readFile "day6.txt"
    case findFirstUnique input of
        Nothing -> putStrLn "No unique characters found"
        Just n -> putStrLn ("First unique characters found at index " ++ show n)
