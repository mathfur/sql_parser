module ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Parser
import Type
import Text.Parsec (ParseError)

instance Eq ParseError where
  x == y = False

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Parser.to_sql "SELECT id,name" ) `shouldBe` (Right $ SQL [SelectStmt [Column "id", Column "name"]])
    it "" $ (Parser.to_sql "SELECT id, name") `shouldBe` (Right $ SQL [SelectStmt [Column "id", Column "name"]])
