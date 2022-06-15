module Main where

import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Char
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as Char
import qualified Data.List                        as List
import           Data.Word

data CsvValue = CsvValue { title :: String, value :: String } deriving (Show, Eq)
data CsvRow = CsvRow { values :: [CsvValue] } deriving (Show, Eq)
data Csv = Csv { rows :: [CsvRow] } deriving (Show, Eq)

main :: IO ()
main = do
    csvFile <- BS.readFile "sample1.csv"
    let parsedCsv = parseOnly parseCsv csvFile
    case parsedCsv of
        Left e    -> error e
        Right parsedCsv -> do
            let csv = parsedCsvToCsv parsedCsv
            print csv

-- Parse CSV Document
parseCsv :: Parser ([BS.ByteString], [[BS.ByteString]])
parseCsv = do
    headerRow <- parseRow
    allRows <- manyTill parseRow endOfInput
    pure (headerRow, allRows)

-- Parse CSV Row, `"Aubrey Huff", "BAL", "Third Baseman", 76, 231, 30.19`
parseRow :: Parser [BS.ByteString]
parseRow = do
    row <- manyTill (parseValue `sepBy1'` Char.char ',') (choice [Char.char '\n', Char.char '\r'])
    pure $ head row -- TODO: Why Is List Wrapped In A List?

-- Parse CSV Value Such As: `"hello"` or `Hello`
parseValue :: Parser BS.ByteString
parseValue = do
    skipWhile (\word -> isQuote word || isSpace word)
    value <- takeTill (\word -> isQuote word || isComma word || endOfLine word)
    skipWhile (\word -> isQuote word || isSpace word)
    pure value
    where
        endOfLine :: Word8 -> Bool
        endOfLine = inClass "\r\n"
        isQuote :: Word8 -> Bool
        isQuote = inClass "\""
        isSpace :: Word8 -> Bool
        isSpace = inClass " "
        isComma :: Word8 -> Bool
        isComma = inClass ","

parsedCsvToCsv :: ([Char.ByteString], [[Char.ByteString]]) -> Csv
parsedCsvToCsv (headerRow, allRows) =
    let
        rows = map (\row ->
            let values = map (\value ->
                        let
                            Just valueIndex = List.elemIndex value row
                            title = headerRow List.!! valueIndex
                        in
                            CsvValue { title = Char.unpack title, value = Char.unpack value }
                    ) row
            in CsvRow { values = values }
            ) allRows
    in
        Csv { rows = rows }
