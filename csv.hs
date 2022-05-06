{-# LANGUAGE OverloadedStrings,  FlexibleContexts   #-}

{-----------------------
AUTHOR: github.com/Tomas-AJF
------------------------}

module CSV where

import Data.List
import Data.Word
import Data.Either
import Control.Monad
import Control.Applicative
import Data.Char (isLetter)
import Data.Attoparsec.Char8
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Strings as S
import qualified Data.Attoparsec.Internal.Types as P 


data CSVValue  =   KEY   B.ByteString 
                 | VALUE B.ByteString   deriving Show
data CSVRow    = CSVRow    [[CSVValue]] deriving Show
data ParsedCSV = ParsedCSV [CSVRow   ]  deriving Show


-- | Extract the Either value that was returned by a Parser
dropEither :: Either a p -> p
dropEither x = case x of
                         Left  _     ->  error "Error inside the parser application"
                         Right value ->  value


-- | Receives a string and try to extract the KEY OR VALUE of a CSV file.
getKeyOrValueOfCSV :: (B.ByteString -> a) -> P.Parser B.ByteString [[a]]
getKeyOrValueOfCSV (csv_Type) = many1 $ stringKeyOfCSV <|> digitKeyOfCSV
                   where                      
                      stringKeyOfCSV = do
                                        ignoreSpace
                                        text <- takeWhile1 isLetterOrParens <* ignoreSpace
                                        return $ [csv_Type text]
                      digitKeyOfCSV = do
                                        ignoreSpace
                                        val <- takeWhile1 isDotOrDigit <* ignoreSpace 
                                        return $ [csv_Type val]

                      -- | Skip all spaces inside the string:
                      ignoreSpace = many $ satisfy $ inClass [ ' ' , '\t', ',', '\n']
                       
                      -- | Checks if a character is equal to a letter or parentheses.
                      isLetterOrParens = \ch ->  (isLetter ch) || (ch == ' ') || (isParens ch) 
                                        where
                                            -- | Checks if a character is equal to a parentheses.
                                               isParens  = \ch ->  elem ch [ '(' , ')' ]
                      
                      -- | Checks if a character is equal to a number or dot.
                      isDotOrDigit = \ch -> (isDigit ch) || ( ch == ' ') || (isDot ch)
                                     where
                                       -- | Checks if a character is equal to a dot.                       
                                          isDot  = \ch -> (ch == '.')
                      
                                                
-- | Receives two CSVValue's (one with key and other a list of CSVValue) and try to join them to create a ROW.
joinColumnWithRows :: [[CSVValue]] -> [[CSVValue]] -> [CSVRow]
joinColumnWithRows _       [] = []
joinColumnWithRows columns rows
                 | (lengthOfColumns > 0) && (lengthOfRows > 0) =  createROW : (joinColumnWithRows columns othersRows)  
                 | otherwise                                   =  error "Colums and rows have different sizes" 
                    where
                         lengthOfColumns =  length columns
                         lengthOfRows    =  length rows
                         firstRow        =  L.take lengthOfColumns rows
                         othersRows      =  L.drop lengthOfColumns rows
                         createROW       =  CSVRow $ zipWith (++) columns firstRow


parsedColumsAndRows :: P.Parser  B.ByteString ([[CSVValue]], [[CSVValue]])
parsedColumsAndRows = do 
          -- | Using the first line of file create the columns with the KEYS: 
          startOfFile  <- S.bytes <$> manyTill anyChar (try $ string "\n")
          -- | Using the rest lines of the file, create the VALUES:
          restOfFile   <- many $ getKeyOrValueOfCSV VALUE
          let startOfFile' = dropEither $ parseOnly (many $ getKeyOrValueOfCSV KEY) startOfFile
          return $ (head $ startOfFile', head restOfFile) 
             

--- PATH OF FILE: CHANGE IT HERE
csvFile :: FilePath
csvFile = "name_of_csv_file.csv"


csvMAIN :: IO ParsedCSV
csvMAIN = do         
             file <- readFile csvFile
             let
                 -- | Remove all double quotes inside of a string:
                 rmDoubleQuotes = \str -> map (filter (/= '\"')) str             
                 
                 -- | Extract all lines from a file, remove any double quotes and convert it to ByteString:
                 restOfFile  =  B.pack $ unlines $ rmDoubleQuotes $ lines $ file             
                 -- | Extract keys and values inside of the file creating columns and rows:                
                 (column, rows) = dropEither $ parseOnly parsedColumsAndRows restOfFile     
                                         
                 -- let getRowsAndColumns = ParsedCSV $ joinColumnWithRows column rows   
                 in return $ ParsedCSV $ joinColumnWithRows column rows
                 
                 where
                 -- | Provide a clean output: remove the ParsedCSV and print the ROWS.
                 cleanCSV csv = cleanCSVOutPut $ csv
                             where
                               cleanCSVOutPut (ParsedCSV csvRowList@(_)) = mapM_ (putStrLn . show ) csvRowList
                               
                               
