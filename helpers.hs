module Helpers (
    commonElt,
    split,
    splitInto,
    splitAt2,
    sumMaybe,
    superlines,

    countWith,
    isSubset,

    tuple2,
    tuple3,
    fst3,
    snd3,
    thd3,

    (<.>),
    (.-.),

    ListZipper,
    getZipper,
    getList,
    goForward,
    goBack,
    steps,
    getElt,
    replaceElt,
    mapElt
) where

import Data.List (intersect)
import Data.List.Split (splitOn)


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

splitAt2 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt2 n m xs = let n' = min n m;  m' = max n m in (take n' xs, take (m' - n') $ drop n' xs, drop m' xs)


sumMaybe :: (Num a) => [Maybe a] -> Maybe a
sumMaybe = sum <.> sequence


superlines :: String -> ([String], [String])
superlines = (id, tail) .-. break null . lines



isSubset :: (Eq a) => ([a], [a]) -> Bool
isSubset = (==) <$> fst <*> uncurry intersect

countWith :: (a -> Bool) -> [a] -> Int
countWith = (length .) . filter

tuple2 :: [a] -> Maybe (a, a)
tuple2 [x, y] = Just (x, y)
tuple2 _ = Nothing

tuple3 :: [a] -> Maybe (a, a, a)
tuple3 [x, y, z] = Just (x, y, z)
tuple3 _ = Nothing


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z


(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <.> f = fmap g . f
infixr 9 <.>

(.-.) :: (b -> d, c -> e) -> (a -> (b, c)) -> (a -> (d, e))
(g1, g2) .-. f = \x -> let (y, z) = f x in (g1 y, g2 z)
infixr 9 .-.


type ListZipper a = ([a],[a])

getZipper :: [a] -> ListZipper a
getZipper = (,[])

getList :: ListZipper a -> [a]
getList (xs, []) = xs
getList (xs, b:bs) = getList (b:xs, bs)

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward (x:xs, bs) = Just (xs, x:bs)
goForward _ = Nothing

goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (xs, b:bs) = Just (b:xs, bs)
goBack _ = Nothing

steps :: Int -> ListZipper a -> Maybe (ListZipper a)
steps n zipper
    | n > 0 = goForward zipper >>= steps (n - 1)
    | n < 0 = goBack zipper >>= steps (n + 1)
    | otherwise = Just zipper

getElt :: ListZipper a -> Maybe a
getElt (x:_, _) = Just x
getElt _ = Nothing

replaceElt :: a -> ListZipper a -> Maybe (ListZipper a)
replaceElt x (_:xs, bs) = Just (x:xs, bs)
replaceElt _ _ = Nothing

mapElt :: (a -> a) -> ListZipper a -> Maybe (ListZipper a)
mapElt f (x:xs, bs) = Just (f x:xs, bs)
mapElt _ _ = Nothing
