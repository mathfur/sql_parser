import qualified Parser
import qualified Formatter

main :: IO ()
main = do
  sql <- getContents
  putStrLn $ Formatter.format $ Parser.parse sql
