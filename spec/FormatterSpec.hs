module FormatterSpec where

import Test.Hspec
import qualified Formatter
import Type

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Formatter.format $ SQL [
               SelectStmt [ResultColumn "id", ResultColumn "name"] (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
            ])
      `shouldBe`
      "SELECT id,name FROM users"
    it "" $ (Formatter.format $ SQL [
               SelectStmt [ResultColumn "*"] (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
            ])
      `shouldBe`
      "SELECT * FROM users"
    it "" $ (Formatter.format $ SQL [
                SelectStmt [ResultColumn "*"] (
                  JoinSource
                    (TableNameSingleSource (TableName Nothing "users") Nothing)
                    [LatterSource Outer (TableNameSingleSource (TableName Nothing "emails") Nothing) (OnConstraint "emails.user_id = users.id")]
                )
              ])
      `shouldBe`
      "SELECT * FROM users LEFT JOIN emails ON emails.user_id = users.id"
