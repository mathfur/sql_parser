module Formatter where

import Type
import Data.List

joinBySp :: [String] -> String
joinBySp strs = intercalate " " strs

class Formattable a where
  format :: a -> String

instance Formattable SQL where
  format (SQL select_stmts) = intercalate ";" $ map format select_stmts

instance Formattable SelectStmt where
  format (SelectStmt columns join_source) = joinBySp [
      "SELECT",
      intercalate "," $ map format columns,
      "FROM",
      format join_source
    ]

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
