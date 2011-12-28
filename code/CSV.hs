-- CSV Code
-- based on code from real world haskell book, ch 16.

module CSV (csvFile, 
            parse, 
            parseCSV, 
            CSV,
            Record,
            Field
           ) where

import Text.ParserCombinators.Parsec

type CSV = [Record]
type Record = [Field]
type Field = String

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

-- 
