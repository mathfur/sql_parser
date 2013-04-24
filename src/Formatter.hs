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
    format (UnionAllOp select_core1 select_core2) = format select_core1 ++ " UNION ALL " ++ format select_core2
    format (UnionOp    select_core1 select_core2) = format select_core1 ++ " UNION "     ++ format select_core2
    format (SelectCore select_option columns join_source where_term group_term) = joinBySp $ [
        from_option select_option,
        intercalate "," $ map format columns,
        "FROM",
        format join_source
      ] ++ (addPrefixA "WHERE " $ fmap format where_term)
        ++ (addPrefixA "GROUP BY " $ fmap format group_term)
       where
         from_option opt = case opt of
             Just SelectDistinct -> "SELECT DISTINCT"
             Just SelectAll      -> "SELECT ALL"
             Nothing             -> "SELECT"

instance Formattable WhereTerm where
    format (WhereTerm expr) = format expr

instance Formattable GroupTerm where
    format (GroupTerm exprs expr) = (intercalate "," $ map format exprs) ++ (addPrefix " HAVING " $ fmap format expr)

instance Formattable ResultColumn where
    format (ResultColumn (Just table_name)) = format table_name ++ ".*"
    format (ResultColumn Nothing) = "*"
    format (ResultColumnExpr expr (Just table_alias)) = format expr ++ " AS " ++ format table_alias
    format (ResultColumnExpr expr Nothing) = format expr

instance Formattable ColumnAlias where
    format (ColumnAlias table_alias) = table_alias

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

instance Formattable DbNameAndTableName where
  format (DbNameAndTableName (Just db_name) table_name) = format db_name ++ "." ++ format table_name
  format (DbNameAndTableName Nothing table_name) = format table_name

instance Formattable Expr where
    format (ColumnNameExpr (Just db_name) (Just table_name_) column_name)  = format db_name ++ "." ++ format table_name_ ++ "." ++ format column_name
    format (ColumnNameExpr Nothing (Just table_name_) column_name) = format table_name_ ++ "." ++ format column_name
    format (ColumnNameExpr (Just db_name) Nothing column_name) = format db_name ++ "." ++ format column_name
    format (ColumnNameExpr Nothing Nothing column_name) = format column_name
    format (UnaryOperatoredExpr unary_operator expr) = format unary_operator ++ " " ++ format expr
    format (PlusOp expr1 expr2) = "(" ++ format expr1 ++ " + " ++ format expr2 ++ ")"
    format (MinusOp expr1 expr2) = "(" ++ format expr1 ++ " - " ++ format expr2 ++ ")"
    format (OrOp expr1 expr2) = "(" ++ format expr1 ++ " OR " ++ format expr2 ++ ")"
    format (MultipleOp expr1 expr2) = format expr1 ++ " * " ++ format expr2
    format (DivideOp expr1 expr2) = format expr1 ++ " / " ++ format expr2
    format (AndOp expr1 expr2) = format expr1 ++ " AND " ++ format expr2
    format (EqualExpr expr1 expr2) = format expr1 ++ " = " ++ format expr2
    format (NullExpr expr) = format expr ++ " IS NULL"
    format (NotNullExpr expr) = format expr ++ " IS NOT NULL"
    format (InExpr expr (Just not) inner_in_expr) = format expr ++ " NOT IN (" ++ format inner_in_expr ++ ")"
    format (InExpr expr Nothing inner_in_expr) = format expr ++ " IN (" ++ format inner_in_expr ++ ")"
    format (LiteralValue literal_value) = format literal_value
    format (FunctionCall function_name exprs) = function_name ++ "(" ++ (intercalate ", " $ map format exprs) ++ ")"
    format (LikeExpr (Just not) expr1 expr2) = format expr1 ++ " NOT LIKE " ++ format expr2
    format (LikeExpr Nothing expr1 expr2) = format expr1 ++ " LIKE " ++ format expr2
    format (BetweenExpr expr1 (Just not) expr2) = format expr1 ++ " NOT BETWEEN " ++ format expr2
    format (BetweenExpr expr1 Nothing expr2)    = format expr1 ++ " BETWEEN "     ++ format expr2

instance Formattable InnerInExpr where
    format (InnerInExprs exprs)                      = intercalate ", " $ map format exprs
    format (InnerInTableName db_name_and_table_name) = format db_name_and_table_name

instance Formattable LiteralValue where
    format (NumericLiteral str) = str
    format (StringLiteral str)  = "\"" ++ escape_inner_string(str) ++ "\""
    format Null                 = "NULL"

instance Formattable UnaryOperator where
    format NotOp = "NOT"

instance Formattable TableName where
  format (TableName str) = str

instance Formattable DbName where
  format (DbName str) = str

escape_inner_string :: String -> String
escape_inner_string = concatMap (\c -> if c == '"' then "\\\"" else [c])
