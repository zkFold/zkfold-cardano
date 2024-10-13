module Bench.Utils (printCSVWithHeaders, writeCSV) where

import           Data.List   (transpose)
import           Prelude
import           Text.Printf (printf)


-- Write benchmark data to a CSV file
writeCSV :: FilePath -> [(Int, Integer, Integer)] -> IO ()
writeCSV path tuples = do
    let csvLines = map (\(n, x, y) -> show n ++ "," ++ show x ++ "," ++ show y) tuples
    writeFile path (unlines csvLines)

-- Read CSV file, printing data together with provided headers
printCSVWithHeaders :: FilePath -> [String] -> IO ()
printCSVWithHeaders filepath headers = do
    contents <- readFile filepath
    let rows             = lines contents
        csvData          = map (splitOn ',') rows
        table            = headers : csvData
        colWidths        = map (maximum . map length) (transpose table)
        formattedHeaders = formatRow colWidths headers
        formattedRows    = map (formatRow colWidths) csvData
    putStrLn formattedHeaders
    mapM_ putStrLn formattedRows


----- HELPER FUNCTIONS -----

-- Format a single row, aligning each column based on calculated widths
formatRow :: [Int] -> [String] -> String
formatRow widths row = unwords [printf ("%-" ++ show width ++ "s") col | (width, col) <- zip widths row]

-- Split a string on a delimiter
splitOn :: Char -> String -> [String]
splitOn delim str = case break (== delim) str of
    (before, "")      -> [before]
    (before, _:after) -> before : splitOn delim after
