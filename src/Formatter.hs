module Formatter where

import Type
import Data.List
import Data.Maybe

joinBySp :: [String] -> String
joinBySp strs = intercalate " " strs

addPrefix :: String -> Maybe String -> String
addPrefix prefix (Just cs) = prefix ++ cs
addPrefix _        Nothing = ""

addPrefixA :: String -> Maybe String -> [String]
addPrefixA prefix (Just cs) = [prefix ++ cs]
addPrefixA _        Nothing = []

class Formattable a where
  format :: a -> String

instance Formattable SQL where
  format (SQL select_stmts) = intercalate "; " $ map format select_stmts

instance Formattable SelectStmt where
  format (SelectStmt select_core ordering_term limit_term) = joinBySp $
    [format select_core] ++
    (if length ordering_term == 0 then [] else ["ORDER BY " ++ (intercalate "," $ map format ordering_term)])
    ++ (map format $ maybeToList limit_term)

instance Formattable OrderingTerm where
  format (OrderingTerm expr order) = joinBySp [
      format expr,
      format order
    ]

instance Formattable Order where
  format Asc = "ASC"
  format Desc = "DESC"

instance Formattable LimitTerm where
  format (LimitTerm expr Nothing) = "LIMIT " ++ format expr
  format (LimitTerm expr (Just offset)) = "LIMIT " ++ format expr ++ "," ++ format offset

instance Formattable SelectCore where
  format (SelectCore columns join_source where_term group_term) = joinBySp $ [
      "SELECT",
      intercalate "," $ map format columns,
      "FROM",
      format join_source
    ] ++ (addPrefixA "WHERE " $ fmap format where_term)
      ++ (addPrefixA "GROUP BY " $ fmap format group_term)

instance Formattable WhereTerm where
  format (WhereTerm expr) = format expr

instance Formattable GroupTerm where
  format (GroupTerm exprs expr) = (intercalate "," $ map format exprs) ++ (addPrefix " HAVING " $ fmap format expr)

instance Formattable ResultColumn where
  format (ResultColumn str) = str

instance Formattable JoinSource where
  format (JoinSource single_source latter_sources) = format single_source ++ spacer ++ latter_string
    where
      latter_string = intercalate " " $ map format latter_sources
      spacer = if (length latter_string == 0) then "" else " "

instance Formattable SingleSource where
  format (JoinSingleSource join_source) = format join_source
  format (TableNameSingleSource table_name Nothing) = format table_name
  format (TableNameSingleSource table_name (Just alias_name)) = joinBySp [
      format table_name,
      "AS",
      format alias_name
    ]

instance Formattable LatterSource where
  format (LatterSource op single_source join_constraint) = joinBySp [
      format op,
      format single_source,
      format join_constraint
    ]

instance Formattable JoinOp where
  format Inner = "INNER JOIN"
  format Outer = "LEFT JOIN"

instance Formattable JoinConstraint where
  format (OnConstraint expr) = "ON " ++ format expr
  format (UsingConstraint column_names) = "Using " ++ (joinBySp $ map format column_names)

instance Formattable ColumnName where
  format (ColumnName str) = str

instance Formattable TableAlias where
  format (TableAlias str) = str

instance Formattable TableName where
  format (TableName (Just db_name) table_name) = db_name ++ "." ++ table_name
  format (TableName Nothing table_name) = table_name

instance Formattable Expr where
    format (ColumnNameExpr (Just db_name) (Just table_name_) column_name)  = format db_name ++ "." ++ format table_name_ ++ "." ++ format column_name
    format (ColumnNameExpr Nothing (Just table_name_) column_name) = format table_name_ ++ "." ++ format column_name
    format (ColumnNameExpr (Just db_name) Nothing column_name) = format db_name ++ "." ++ format column_name
    format (ColumnNameExpr Nothing Nothing column_name) = format column_name
    format (UnaryOperatoredExpr unary_operator expr) = format unary_operator ++ " " ++ format expr
    format (PlusOp expr1 expr2) = "(" ++ format expr1 ++ " + " ++ format expr2 ++ ")"
    format (MinusOp expr1 expr2) = "(" ++ format expr1 ++ " - " ++ format expr2 ++ ")"
    format (MultipleOp expr1 expr2) = format expr1 ++ " * " ++ format expr2
    format (DivideOp expr1 expr2) = format expr1 ++ " / " ++ format expr2
    format (NullExpr expr) = format expr ++ " IS NULL"
    format (NotNullExpr expr) = format expr ++ " IS NOT NULL"
    format (InExpr expr (Just not) inner_in_expr) = format expr ++ " NOT IN (" ++ format inner_in_expr ++ ")"
    format (InExpr expr Nothing inner_in_expr) = format expr ++ " IN (" ++ format inner_in_expr ++ ")"
    format (LiteralValue literal_value) = format literal_value
    format (FunctionCall function_name exprs) = function_name ++ "(" ++ (intercalate ", " $ map format exprs) ++ ")"

instance Formattable InnerInExpr where
    format (InnerInExprs exprs)                   = intercalate ", " $ map format exprs
    format (InnerInTableName (Just db_name) table_name) = format db_name ++ "." ++ format table_name
    format (InnerInTableName Nothing table_name) = format table_name

instance Formattable LiteralValue where
    format (NumericLiteral str) = str
    format (StringLiteral str)  = "\"" ++ escape_inner_string(str) ++ "\""
    format Null                 = "NULL"

instance Formattable UnaryOperator where
    format NotOp = "NOT"

instance Formattable TableName_ where
  format (TableName_ str) = str

instance Formattable DbName where
  format (DbName str) = str

escape_inner_string :: String -> String
escape_inner_string = concatMap (\c -> if c == '"' then "\\\"" else [c])
