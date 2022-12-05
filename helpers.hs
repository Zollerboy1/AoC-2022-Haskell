module Helpers (commonElt, countWith, isSubset, split, splitInto, sumMaybe, tuple2, (<.>)) where

import Data.List (intersect)


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



isSubset :: (Eq a) => ([a], [a]) -> Bool
isSubset = (==) <$> fst <*> uncurry intersect

countWith :: (a -> Bool) -> [a] -> Int
countWith = (length .) . filter

tuple2 :: [a] -> Maybe (a, a)
tuple2 [x, y] = Just (x, y)
tuple2 _ = Nothing


(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <.> f = fmap g . f
infixr 9 <.>
