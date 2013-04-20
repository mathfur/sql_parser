module Parser where

import Text.Parsec (choice, char, string, alphaNum, sepBy, spaces, many1, parse, ParseError, optionMaybe, try, between, noneOf, digit, oneOf, (<?>), eof )
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Control.Applicative

import Type

sql :: Parser SQL
sql = SQL <$> (select_stmt `sepBy` (c ';'))

select_stmt :: Parser SelectStmt
select_stmt = SelectStmt <$> select_core <*> (many $ try ordering_term) <*> (optionMaybe limit_term)

limit_term :: Parser LimitTerm
limit_term = LimitTerm <$> (s *> str "LIMIT" *> s *> expr) <*> optionMaybe (s*> str "," *> expr)

select_core :: Parser SelectCore
select_core = SelectCore <$>
    (s *> str "SELECT" *> s *> (result_column `sepBy` (c ',')) <* s <* str "FROM" <* s)
    <*>
    join_source
    <*>
    (optionMaybe (try where_term))
    <*>
    (optionMaybe (try group_term))

result_column :: Parser ResultColumn
result_column = ResultColumn <$> (s *> many1 column_name_ch <* s)

where_term :: Parser WhereTerm
where_term = WhereTerm <$> (s *> str "WHERE" *> s *> expr)

group_term :: Parser GroupTerm
group_term = GroupTerm <$> (s *> str "GROUP" *> s *> str "BY" *> (expr `sepBy` (c ','))) <*> (optionMaybe (str "HAVING" *> expr))

ordering_term :: Parser OrderingTerm
ordering_term = OrderingTerm <$> (s *> str "ORDER" *> s *> str "BY" *> s *> expr) <*> order

order :: Parser Order
order = do
  order_string <- (s *> choice [str "ASC", str "DESC", str ""] <* s)
  case order_string of
    "DESC" -> return Desc
    _ -> return Asc

join_source :: Parser JoinSource
join_source = JoinSource <$> single_source <*> (many (try latter_source))

latter_source :: Parser LatterSource
latter_source = LatterSource <$>
    (join_op <* s <* str "JOIN" <* s ) <*>
    single_source <*>
    join_constraint
  where
    join_op :: Parser JoinOp
    join_op = do
      left <- optionMaybe (s *> str "LEFT" <* s)
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

join_constraint :: Parser JoinConstraint
join_constraint = wrap_sp $
  OnConstraint    <$> ((str "ON") *> s *> expr)
  <|>
  UsingConstraint <$> ((str "USING") *> s *> (many1 $ try column_name))

table_alias :: Parser TableAlias
table_alias = wrap_sp $ TableAlias <$> many1 table_alias_ch

literal_value :: Parser LiteralValue
literal_value = string_literal
            <|> null_literal
            <|> numeric_literal

---------------------------
expr :: Parser Expr
expr = wrap_sp ( buildExpressionParser table factor ) <?> "expr"

table = [[prefix (try $ str "NOT") (UnaryOperatoredExpr NotOp)]
        ,[postfix (try $ str "IS" <* s <* str "NOT" <* s <* str "NULL") NotNullExpr]
        ,[postfix (try $ str "IS" <* s <* str "NULL") NullExpr]
        ,[in_expr]
        ,[like_expr]
        ,[between_expr]
        ,[binary (str "*") MultipleOp AssocLeft, binary (str "/") DivideOp AssocLeft]
        ,[binary (str "+") PlusOp AssocLeft,     binary (str "-") MinusOp AssocLeft]
        ]
      where
        binary  pattern fun assoc = Infix (do{ pattern; return fun }) assoc
        prefix  pattern fun       = Prefix (do{ pattern; return fun })
        postfix pattern fun       = Postfix (do{ pattern; return fun })
        in_expr = Postfix (do
               not <- try(not_op <* s <* str "IN" <* s)
               in_parenthesis <- (s *> str "(" *> db_name_and_table_name <* str ")" <* s)
               return $ (flip ((flip InExpr) not) in_parenthesis)
               )
        like_expr = Infix (do
                     not <- try(not_op <* s <* str "LIKE" <* s)
                     return (LikeExpr not)
                     ) AssocLeft
        between_expr = Postfix (do
                               not <- try(not_op <* s <* str "BETWEEN" <* s)
                               expr1 <- expr <* s <* str "AND" <* s
                               expr2 <- expr <* s
                               return $ flip (flip (flip BetweenExpr not) expr1) expr2
                               )

factor :: Parser Expr
factor = (try $ s *> str "(" *> expr <* str ")" <* s) <|> term <?> "factor"

not_op :: Parser (Maybe UnaryOperator)
not_op = wrap_sp $ fmap (\_ -> NotOp) <$> optionMaybe(try $ str "NOT")

term :: Parser Expr
term = (LiteralValue <$> try(literal_value))
    <|> try(function_call)
    <|> try(column_name_literal)
    <?> "term"

---------------------------

function_call :: Parser Expr
function_call = FunctionCall <$> many1 function_name_ch <*> (str "(" *> (expr `sepBy` c ',') <* str ")" )

numeric_literal :: Parser LiteralValue
numeric_literal = wrap_sp $ (\num decimal -> NumericLiteral $ (maybe num ((num ++ ".") ++) decimal)) <$> many1 digit <*> optionMaybe (c '.' *> many1 digit)

string_literal :: Parser LiteralValue
string_literal = wrap_sp $ StringLiteral <$> between (c '"') (c '"') (many inner_char)
  where
    inner_char = unescaped <|> escaped
    unescaped = noneOf "\"\\"
    escaped = c '\\' >> (oneOf "\"\\")

null_literal :: Parser LiteralValue
null_literal = do
    wrap_sp $ try $ str "NULL"
    return Null

column_name_literal :: Parser Expr
column_name_literal = wrap_sp $
    ColumnNameExpr <$> (optionMaybe (try $ DbName     <$> many1 db_name_ch    <* str "."))
                   <*> (optionMaybe (try $ TableName_ <$> many1 table_name_ch <* str "."))
                   <*> (ColumnName <$> many1 column_name_ch)

-------------------------------------------------
column_name :: Parser ColumnName
column_name = wrap_sp $ ColumnName <$> many1 column_name_ch

table_name :: Parser TableName
table_name = wrap_sp $
    TableName <$> optionMaybe (try $ many1 table_name_ch <* str ".")
              <*> many1 column_name_ch

db_name_and_table_name :: Parser InnerInExpr
db_name_and_table_name = wrap_sp $
    InnerInExprs     <$> expr `sepBy` c ','
    <|>
    InnerInTableName <$> (optionMaybe (try $ DbName <$> many1 db_name_ch    <* str "."))
                     <*> (TableName_   <$> many1 table_name_ch)

-------------------------------------------------
db_name_ch :: Parser Char
db_name_ch = oneOf "_" <|> alphaNum

table_name_ch :: Parser Char
table_name_ch = oneOf "_" <|> alphaNum

column_name_ch :: Parser Char
column_name_ch = oneOf "_" <|> alphaNum

function_name_ch :: Parser Char
function_name_ch = oneOf "_" <|> alphaNum

table_alias_ch :: Parser Char
table_alias_ch = oneOf "_" <|> alphaNum

wrap_sp :: Parser a -> Parser a
wrap_sp parser = (s *> parser <* s)

-------------------------------------------------
-- | aliases

s :: Parser ()
s = spaces

c :: Char -> Parser Char
c = char

str :: String -> Parser String
str = string

-------------------------------------------------
to_sql :: String -> Either ParseError SQL
to_sql src = parse sql "(sourcename)" src

-- FOR TEST
for_test :: String -> Either ParseError SQL
for_test src = parse sql "(sourcename)" src
