module ParserSpec where

import Test.Hspec
import qualified Parser
import Type
import Text.Parsec (ParseError)

instance Eq ParseError where
  _ == _ = False

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Parser.to_sql "SELECT id,name FROM users")  `shouldBe` (Right $ SQL [SelectStmt [ResultColumn "id", ResultColumn "name"] (JoinSource "users")])
    it "" $ (Parser.to_sql "SELECT id, name FROM users") `shouldBe` (Right $ SQL [SelectStmt [ResultColumn "id", ResultColumn "name"] (JoinSource "users")])
    it "" $ (Parser.to_sql "SELECT * FROM users")        `shouldBe` (Right $ SQL [SelectStmt [ResultColumn "*"]                       (JoinSource "users")])
