import Parser (to_sql)
import Formatter ()
import Type

main :: IO ()
main = do
  --sql <- getContents
  --putStrLn $ Formatter.format $ Parser.parse sql
  case (Parser.to_sql "SELECT id,hoge") of
    Right e -> print e
    Left e -> do
      print "(error)"
      print e
