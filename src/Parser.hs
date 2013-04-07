module Parser where

import Text.Parsec (char, string, alphaNum, sepBy, spaces, many1, parseTest, parse, ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative

import Type

sql :: Parser SQL
sql = SQL <$> (select_stmt `sepBy` (c ';'))

select_stmt :: Parser SelectStmt
select_stmt = SelectStmt <$> (str "SELECT" *> ((s *> column <* s) `sepBy` (c ',')))

column :: Parser Column
column = Column <$> (many1 alphaNum)

s = spaces
c = char
str = string

-------------------------------------------------
to_sql :: String -> Either ParseError SQL
to_sql src = parse sql "(sourcename)" src
