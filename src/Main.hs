import Parser (to_sql)
import Formatter (format)
import Type

main :: IO ()
main = do
  sql <- getContents
  case (Parser.to_sql sql) of
    Right e -> putStrLn $ Formatter.format e
    Left e -> do
      print "(error)"
      print e
