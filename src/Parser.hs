module Parser where

import Text.Parsec (char, string, alphaNum, sepBy, spaces, many1, parseTest, parse, ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative

import Type

sql :: Parser SQL
sql = SQL <$> (select_stmt `sepBy` (c ';'))

select_stmt :: Parser SelectStmt
select_stmt = SelectStmt <$>
    (str "SELECT" *> (result_column `sepBy` (c ',')) <* str "FROM")
    <*>
    join_source

result_column :: Parser ResultColumn
result_column = ResultColumn <$> (s *> many1 column_character <* s)
  where
    column_character = char '*' <|> alphaNum

join_source :: Parser JoinSource
join_source = JoinSource <$> (s *> many1 alphaNum <* s)

s :: Parser ()
s = spaces
c = char

str :: String -> Parser [Char]
str = string

-------------------------------------------------
to_sql :: String -> Either ParseError SQL
to_sql src = parse sql "(sourcename)" src
