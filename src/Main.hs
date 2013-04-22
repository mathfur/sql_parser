import Parser (to_sql, for_test)
import Formatter (format)
import Type
import ProcessSql
import System.IO (getContents)
import Control.Monad

get_line_and_output :: IO ()
get_line_and_output = do
    sql <- getLine
    case (Parser.to_sql sql) of
      Right e -> do
        let result = Formatter.format e
        putStrLn $ "OK(" ++ show (length result) ++ "): " ++ show sql ++ ": " ++ show result
      Left e -> do
        putStrLn $ "NG: " ++ show sql

main :: IO ()
main = forever get_line_and_output
