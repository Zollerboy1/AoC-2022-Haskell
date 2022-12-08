import Data.Char (ord)
import Data.List (findIndex, inits, tails, transpose)
import Data.Maybe (fromMaybe)

import Helpers (countWith)


type DirList = [[Int]]

parseGrid :: String -> [[Int]]
parseGrid = map (map ((-) <$> ord <*> pure (ord '0'))) . lines

dirLists :: [[Int]] -> [DirList]
dirLists = (map (uncurry (++)) . (zip <$> concat . transpose . leftAndRight . transpose <*> concat . leftAndRight))
    where combine (r, l, h) = [map (subtract h)] <*> [r, l]
          leftAndRight = map (map combine . (zip3 <$> map reverse . init . inits <*> tail . tails <*> id))

-- Completely point free but I like the upper version more.
-- dirLists :: [[Int]] -> [DirList]
-- dirLists = (map (uncurry (++)) . (zip <$> concat . transpose . leftAndRight . transpose <*> concat . leftAndRight))
--     where combine = (<*>) . return . map . subtract
--           leftAndRight = map (map (uncurry combine) . (zip <$> id <*> (((transpose .) . (. return) . (:)) <$> map reverse . init . inits <*> tail . tails)))


part1 :: [DirList] -> Int
part1 = countWith (any (all (<0)))

part2 :: [DirList] -> Int
part2 = maximum . map (foldr1 (*) . map (fromMaybe <$> (length) <*> (fmap (+1) . findIndex (>=0))))


main :: IO ()
main = do
    input <- readFile "day8.txt"
    let dirLists' = dirLists $ parseGrid input
    print $ part1 dirLists'
    print $ part2 dirLists'
