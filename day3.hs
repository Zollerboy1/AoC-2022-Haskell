import Data.Char (ord)
import Data.List.Split (chunksOf)

commonElt :: String -> String -> Char
commonElt (x:xs) ys
    | x `elem` ys = x
    | otherwise = commonElt xs ys

commonElt3 :: String -> String -> String -> Char
commonElt3 (x:xs) ys zs
    | x `elem` ys && x `elem` zs = x
    | otherwise = commonElt3 xs ys zs

priority :: Char -> Int
priority c
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

part1 :: [String] -> Int
part1 = sum . map priority . map (\(a, b) -> commonElt a b) . map (\rucksack -> splitAt (length rucksack `div` 2) rucksack)

part2 :: [String] -> Int
part2 = sum . map priority . map (\[a, b, c] -> commonElt3 a b c) . chunksOf 3


main :: IO()
main = do
    input <- readFile "day3.txt"
    let lines' = lines input
    putStrLn (show $ part1 lines')
    putStrLn (show $ part2 lines')
