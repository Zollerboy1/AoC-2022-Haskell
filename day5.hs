import Helpers (superlines, splitAt2, tuple3, (<.>), ListZipper, getZipper, getList, steps, getElt, replaceElt, mapElt)

import Control.Monad ((<=<))
import Data.List (partition, transpose)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.Read (readMaybe)

type Stack = [Char]
type Command = (Int, Int, Int)

parseStacks :: [String] -> [Stack]
parseStacks = map (dropWhile (==' ') . init . snd) . filter ((==1) . (`mod` 4) . fst) . zip [0..] . transpose

parseCommands :: [String] -> Maybe [Command]
parseCommands = mapM (tuple3 <=< mapM (readMaybe . snd) . snd . partition (even . fst) . zip [0..] . words)

parse :: ([String], [String]) -> Maybe ([Stack], [Command])
parse (s, c) = case (parseStacks s, parseCommands c) of
    (_, Nothing) -> Nothing
    (stacks, Just commands) -> Just (stacks, commands)


performCommand :: (Int -> Stack -> Maybe ([Char], Stack)) -> Command -> [Stack] -> [Stack]
performCommand f (n, src, dst) s = fromJust $ do
    srcZipper <- steps (src - 1) (getZipper s)
    (chunks, sSrc') <- f n =<< getElt srcZipper
    dstZipper <- steps (dst - src) =<< replaceElt sSrc' srcZipper
    getList <$> mapElt (chunks ++) dstZipper

part1 :: Int -> Stack -> Maybe ([Char], Stack)
part1 = flip $ (swap <.>) . flip steps . getZipper


part2 :: Int -> Stack -> Maybe ([Char], Stack)
part2 = (Just .) . splitAt

result :: (Int -> Stack -> Maybe ([Char], Stack)) -> [Stack] -> [Command] -> String
result = ((map head .) .) . foldl . flip . performCommand

main :: IO ()
main = do
    input <- readFile "day5.txt"
    case parse $ superlines input of
        Nothing -> putStrLn "Input is corrupted"
        Just parsed -> do
            putStrLn (show $ uncurry (result part1) parsed)
            putStrLn (show $ uncurry (result part2) parsed)
