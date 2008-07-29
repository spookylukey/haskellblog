import qualified Category as C
import qualified Settings
-- Migration script for the old data

-- Read a table of newline/tab delimited data,
-- padding columns to specified amount
readTable :: FilePath -> Int -> IO [[String]]
readTable filename cols = do f <- readFile filename
                             let lines = filter (/= "") $ splitRows f
                                 arr = map (padCols . splitCols) lines
                             return arr
    where 
      splitRows s = split s '\n'
      splitCols s = split s '\t'
      padCols xs = padTo cols xs ""

-- Pad a list to a given length with a default value
padTo :: Int -> [a] -> a -> [a]
padTo newlength xs defaultVal = let extra = max 0 (newlength - length xs)
                                in xs ++ (take extra $ repeat defaultVal)

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

-- Utility functions that handle null data
-- and return appropriate defaults
readInt :: String -> Int
readInt "" = 0
readInt s = read s

readCategories = do ds <- readTable (Settings.old_data_path ++ "categories.txt") 2
                    let mkCat row = C.Category { C.id = read (row !! 0),
                                                 C.name = row !! 1}
                    return $ map mkCat ds
