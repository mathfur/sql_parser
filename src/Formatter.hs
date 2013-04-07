module Formatter where

import Type
import Data.List

class Formattable a where
  format :: a -> String

instance Formattable SQL where
  format (SQL select_stmts) = intercalate ";" $ map format select_stmts

instance Formattable SelectStmt where
  format (SelectStmt columns join_source) = intercalate " " $ [
      "SELECT",
      (intercalate "," $ map format columns),
      "FROM",
      format join_source
    ]

instance Formattable ResultColumn where
  format (ResultColumn str) = str

instance Formattable JoinSource where
  format (JoinSource str) = str
