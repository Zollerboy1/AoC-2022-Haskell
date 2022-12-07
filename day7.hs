import Control.Monad (foldM, join)
import Data.List (findIndex)
import Data.Maybe (catMaybes, fromJust)
import Text.Read (readMaybe)

data FSNode = File String Int | Dir String Int [FSNode]

nodeHasName :: FSNode -> String -> Bool
nodeHasName (File n _) name = n == name
nodeHasName (Dir n _ _) name = n == name

dirSize :: FSNode -> Maybe Int
dirSize (Dir _ s _) = Just s
dirSize _ = Nothing

type FSZipper = (FSNode, [(String, Int, [FSNode], [FSNode])])

zipperFromNode :: FSNode -> FSZipper
zipperFromNode = (,[])

zipperToNode :: FSZipper -> FSNode
zipperToNode = fst . fsGoRoot

fsGoTo :: FSZipper -> String -> Maybe FSZipper
fsGoTo (Dir dirName s files, bs) name = fmap ((\(l, f:r) -> (f, (dirName, s, l, r):bs)) . (`splitAt` files)) $ findIndex (`nodeHasName` name) files
fsGoTo _ _ = Nothing

fsGoUp :: FSZipper -> Maybe FSZipper
fsGoUp (node, (dirName, s, l, r):bs) = Just (Dir dirName s (l ++ [node] ++ r), bs)
fsGoUp _ = Nothing

fsGoRoot :: FSZipper -> FSZipper
fsGoRoot (node, []) = (node, [])
fsGoRoot (node, (dirName, s, l, r):bs) = fsGoRoot (Dir dirName s (l ++ [node] ++ r), bs)

fsAddSize :: Int -> [(String, Int, [FSNode], [FSNode])] -> [(String, Int, [FSNode], [FSNode])]
fsAddSize s = map (\(n, size, l, r) -> (n, size + s, l, r))

fsAddNode :: FSZipper -> FSNode -> Maybe FSZipper
fsAddNode (Dir dirName dirSize dirFiles, bs) node =
    let size = case node of
            (Dir _ s _) -> s
            (File _ s) -> s
    in Just (Dir dirName (dirSize + size) (node:dirFiles), fsAddSize size bs)
fsAddNode _ _ = Nothing


parseCommand :: FSZipper -> String -> Maybe FSZipper
parseCommand z "cd /" = Just $ fsGoRoot z
parseCommand z "cd .." = fsGoUp z
parseCommand z ('c':'d':' ':name) = fsGoTo z name
parseCommand z "ls" = Just z
parseCommand _ _ = Nothing

parseLine :: FSZipper -> String -> Maybe FSZipper
parseLine z ('$':' ':cmd) = parseCommand z cmd
parseLine z ('d':'i':'r':' ':name) = fsAddNode z (Dir name 0 [])
parseLine z line = do
    (sizeStr, _:name) <- Just $ break (==' ') line
    fsAddNode z =<< (fmap (File name) $ readMaybe sizeStr)

parseTree :: String -> Maybe FSNode
parseTree = (zipperToNode <$>) . foldM parseLine (zipperFromNode $ Dir "/" 0 []) . lines


fsMapNodes :: (FSNode -> a) -> FSNode -> [a]
fsMapNodes f node = case node of
    (File _ _) -> [f node]
    (Dir _ _ files) -> f node:(join $ map (fsMapNodes f) files)

part1 :: FSNode -> Int
part1 = sum . filter (<=100000) . catMaybes . fsMapNodes dirSize

part2 :: FSNode -> Int
part2 node = minimum . filter (>(fromJust (dirSize node) - 40000000)) . catMaybes $ fsMapNodes dirSize node

main :: IO ()
main = do
    input <- readFile "day7.txt"
    case parseTree input of
        Nothing -> putStrLn "Input is corrupted"
        Just fileTree -> do
            putStrLn (show $ part1 fileTree)
            putStrLn (show $ part2 fileTree)
