module Formatter where

import Type
import Data.List
import Data.Maybe

joinBySp :: [String] -> String
joinBySp strs = intercalate " " strs

addPrefix :: String -> Maybe String -> String
addPrefix prefix (Just cs) = prefix ++ cs
addPrefix prefix Nothing = ""

addPrefixA :: String -> Maybe String -> [String]
addPrefixA prefix (Just cs) = [prefix ++ cs]
addPrefixA prefix Nothing = []

class Formattable a where
  format :: a -> String

instance Formattable SQL where
  format (SQL select_stmts) = intercalate ";" $ map format select_stmts

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
  format (SelectCore columns join_source where_term) = joinBySp $ [
      "SELECT",
      intercalate "," $ map format columns,
      "FROM",
      format join_source
    ] ++ (addPrefixA "WHERE " $ fmap format where_term)

instance Formattable WhereTerm where
  format (WhereTerm expr) = format expr

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
  format (OnConstraint str) = "ON " ++ str
  format (UsingConstraint column_names) = "Using " ++ (joinBySp $ map format column_names)

instance Formattable ColumnName where
  format (ColumnName str) = str

instance Formattable TableAlias where
  format (TableAlias str) = str

instance Formattable TableName where
  format (TableName (Just db_name) table_name) = db_name ++ "." ++ table_name
  format (TableName Nothing table_name) = table_name

instance Formattable Expr where
  format (Expr str) = str
