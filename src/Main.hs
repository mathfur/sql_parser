import Parser (to_sql, for_test)
import Formatter (format)
import Type
import ProcessSql
import System.IO (getContents)
import Control.Monad
import Data.Char (toUpper)
import Text.Printf (printf)

get_line_and_output :: IO ()
get_line_and_output = do
    sql <- getLine
    case (Parser.to_sql $ map toUpper sql) of
      Right e -> do
        let result = Formatter.format e
        let diff = abs $ (length sql) - (length result)
        let ratio :: Float
            ratio = ((fromIntegral diff) / (fromIntegral $ length sql))*100
        putStrLn $ "OK(diff:" ++ show diff ++ ", ratio:" ++ printf "%.0f" ratio ++ "):\t" ++ show sql ++ "\n\t\t\t" ++ show result
      Left e -> do
        putStrLn $ "NG: " ++ show sql

main :: IO ()
main = forever get_line_and_output
