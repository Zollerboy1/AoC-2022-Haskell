import Data.Char (ord)

parse :: String -> [(Int, Int)]
parse = map ((\(opp:me:_) -> (ord opp - ord 'A', ord me - ord 'X')) . map head . words) . lines

part1 :: [(Int, Int)] -> Int
part1 = sum . map (\(opp, me) -> 1 + me + (rem (me - opp + 4) 3) * 3)

part2 :: [(Int, Int)] -> Int
part2 = sum . map (\(opp, me) -> 1 + (rem (opp + me + 2) 3) + me * 3)

main :: IO ()
main = do
    input <- readFile "day2.txt"
    let parsed = parse input
    putStrLn (show $ part1 parsed)
    putStrLn (show $ part2 parsed)
