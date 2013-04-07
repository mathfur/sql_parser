module FormatterSpec where

import Test.Hspec
import qualified Formatter
import Type

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Formatter.format (SQL [SelectStmt [ResultColumn "id", ResultColumn "name"] (JoinSource "users")])) `shouldBe` "SELECT id,name FROM users"
    it "" $ (Formatter.format (SQL [SelectStmt [ResultColumn "*"]                       (JoinSource "users")])) `shouldBe` "SELECT * FROM users"
