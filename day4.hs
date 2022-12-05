import Control.Monad ((<=<))
import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Text.Read (readMaybe)

import Helpers (countWith, isSubset, tuple2, (<.>))


parse :: String -> Maybe [([Int], [Int])]
parse = mapM (tuple2 <=< mapM (uncurry enumFromTo <.> tuple2 <=< mapM readMaybe . splitOn "-") . splitOn ",") . lines

part1 :: [([Int], [Int])] -> Int
-- part1 = countWith $ or . (isSubset <$>) . ([id, swap] <*>) . pure
part1 = countWith $ (||) <$> isSubset <*> isSubset . swap

part2 :: [([Int], [Int])] -> Int
part2 = countWith $ not . null . uncurry intersect

main :: IO ()
main = do
    input <- readFile "day4.txt"
    case parse input of
        Nothing -> putStrLn "Input is corrupted"
        Just parsed -> do
            putStrLn $ (show . part1) parsed
            putStrLn $ (show . part2) parsed
