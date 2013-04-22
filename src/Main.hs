import Parser (to_sql, for_test)
import Formatter (format)
import Type
import ProcessSql

main :: IO ()
main = do
  sql <- getContents
  case (Parser.to_sql sql) of
    Right e -> do
      print e
      print $ Formatter.format e
    Left e -> do
      print "(error)"
      print e
