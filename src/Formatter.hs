module Formatter where

import Type
import Data.List

class Formattable a where
  format :: a -> String

instance Formattable SQL where
  format (SQL select_stmts) = intercalate ";" $ map format select_stmts

instance Formattable SelectStmt where
  format (SelectStmt columns) = "SELECT " ++ (intercalate "," $ map format columns)

instance Formattable Column where
  format (Column str) = str
