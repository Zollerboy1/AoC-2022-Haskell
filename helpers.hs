module Helpers (commonElt, split, splitInto, sumMaybe) where


commonElt :: (Eq a) => [[a]] -> a
commonElt (x:xs) = head [elt | elt <- x, and $ map (elem elt) xs]

split :: (Eq a) => a -> [a] -> [[a]]
split sep l = case dropWhile (== sep) l of
                [] -> []
                l' -> p : split sep l''
                    where (p, l'') = break (== sep) l'

splitInto :: Int -> [a] -> [[a]]
splitInto 1 xs = [xs]
splitInto n xs = h : splitInto (n - 1) t
    where l = length xs
          c = l `div` n + if l `mod` n == 0 then 0 else 1
          (h, t) = splitAt c xs

sumMaybe :: (Num a) => [Maybe a] -> Maybe a
sumMaybe = fmap sum . sequence
