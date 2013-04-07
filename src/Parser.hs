module Parser where

import Text.Parsec (choice, char, string, alphaNum, sepBy, spaces, many1, parseTest, parse, ParseError, optionMaybe)
import Text.Parsec.String (Parser)
import Control.Applicative

import Type

sql :: Parser SQL
sql = SQL <$> (select_stmt `sepBy` (c ';'))

select_stmt :: Parser SelectStmt
select_stmt = SelectStmt <$>
    (str "SELECT" *> (result_column `sepBy` (c ',')) <* str "FROM" <* s)
    <*>
    join_source

result_column :: Parser ResultColumn
result_column = ResultColumn <$> (s *> many1 column_character <* s)

join_source :: Parser JoinSource
join_source = JoinSource <$> single_source <*> (many latter_source)
  -- old: (s *> many1 alphaNum <* s)

latter_source :: Parser LatterSource
latter_source = LatterSource <$>
    (join_op <* s <* str "JOIN" <* s ) <*>
    single_source <*>
    join_constraint
  where
    join_op :: Parser JoinOp
    join_op = do
      left <- optional (s *> str "LEFT" <* s)
      outer_or_inner <- (s *> (choice [str "OUTER", str "INNER", str ""]) <* s)
      if (left == Just "LEFT") then
        return Outer
      else if (outer_or_inner == "OUTER") then
        return Outer
      else
        return Inner
    
single_source :: Parser SingleSource    
single_source =
  (TableNameSingleSource <$> table_name <*> (optional (str "AS" *> s *> table_alias))) <|>
  (JoinSingleSource <$> join_source)
    where  
      table_name :: Parser TableName
      table_name =
        (TableName Nothing <$> (many1 alphaNum)) <|>
        (TableName <$> (optionMaybe (s *> (many1 alphaNum) <* s <* str ".")) <*> (many1 alphaNum))

join_constraint :: Parser JoinConstraint
join_constraint =
  OnConstraint <$> (s *> (str "ON") *> s *> (many1 alphaNum) <* s)
  <|>
  UsingConstraint <$> (s *> (str "USING") *> s *> (many1 column_name) <* s)
    where
      column_name :: Parser ColumnName
      column_name = ColumnName <$> (s *> many1 column_character <* s)

table_alias :: Parser TableAlias
table_alias = TableAlias <$> (s *> many1 alphaNum <* s)

-------------------------------------------------
column_character = char '*' <|> alphaNum
-------------------------------------------------
s = spaces
c = char
str = string

-------------------------------------------------
to_sql :: String -> Either ParseError SQL
to_sql src = parse sql "(sourcename)" src

-- FOR TEST
for_test :: String -> Either ParseError SQL
for_test src = parse sql "(sourcename)" src
