module Helpers (split, sumMaybe) where

split :: (Eq a) => a -> [a] -> [[a]]
split sep l = case dropWhile (== sep) l of
                [] -> []
                l' -> p : split sep l''
                    where (p, l'') = break (== sep) l'

sumMaybe :: (Num a) => [Maybe a] -> Maybe a
sumMaybe = fmap sum . sequence
