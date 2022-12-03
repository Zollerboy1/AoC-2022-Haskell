import Helpers (commonElt, splitInto)
import Data.Char (ord)
import Data.List.Split (chunksOf)

priority :: Char -> Int
priority c
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

part1 :: [String] -> [[String]]
part1 = map $ splitInto 2

part2 :: [String] -> [[String]]
part2 = chunksOf 3

answer :: [[String]] -> Int
answer = sum . map (priority . commonElt)

main :: IO ()
main = do
    input <- readFile "day3.txt"
    let lines' = lines input
    putStrLn (show $ answer $ part1 lines')
    putStrLn (show $ answer $ part2 lines')
