import Data.Char (toUpper)
import Data.List.Split (chunksOf)
import Text.Read (readMaybe)

import Helpers (thd3)


data Instr = Noop | Addx Int deriving (Read)

capitalize :: String -> String
capitalize = (:) <$> toUpper . head <*> tail

parseInstructions :: String -> Maybe [Instr]
parseInstructions = mapM (readMaybe . capitalize) . lines

runCPU :: (Int -> Int -> a -> a) -> a -> [Instr] -> a
runCPU f init = thd3 . foldl runInstr (0, 1, init)
    where runInstr (c, v, r) Noop = (c + 1, v, f (c + 1) v r)
          runInstr (c, v, r) (Addx x) = (c + 2, v + x, f (c + 2) v (f (c + 1) v r))


part1 :: [Instr] -> Int
part1 = runCPU f 0
    where f c v
            | c `mod` 40 == 20 = (+ c * v)
            | otherwise = id

part2 :: [Instr] -> String
part2 = unlines . chunksOf 40 . reverse . runCPU f ""
    where f c v
            | abs ((c - 1) `mod` 40 - v) <= 1 = ('#':)
            | otherwise = ('.':)


main :: IO ()
main = do
    input <- readFile "day10.txt"
    case parseInstructions input of
        Nothing -> putStrLn "Input is corrupted"
        Just instructions -> do
            print $ part1 instructions
            putStr $ part2 instructions
