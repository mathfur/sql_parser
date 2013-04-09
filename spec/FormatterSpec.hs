module FormatterSpec where

import Test.Hspec
import qualified Formatter
import Type

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "id", ResultColumn "name"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 Nothing
               )
               []
               Nothing
            ])
      `shouldBe`
      "SELECT id,name FROM users"
    it "" $ (Formatter.format $ SQL [SelectStmt (
                 SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 Nothing
               )
               []
               Nothing
            ])
      `shouldBe`
      "SELECT * FROM users"
    it "" $ (Formatter.format $ SQL [SelectStmt (
                SelectCore [ResultColumn "*"] (
                  JoinSource
                    (TableNameSingleSource (TableName Nothing "users") Nothing)
                    [LatterSource Outer (TableNameSingleSource (TableName Nothing "emails") Nothing) (OnConstraint "emails.user_id = users.id")]
                  )
                  Nothing
                  Nothing
                )
                []
                Nothing
              ])
      `shouldBe`
      "SELECT * FROM users LEFT JOIN emails ON emails.user_id = users.id"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 Nothing
               )
               []
               (Just (LimitTerm (Expr "1") Nothing))
            ])
      `shouldBe`
      "SELECT * FROM users LIMIT 1"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 Nothing
               )
               []
               (Just (LimitTerm (Expr "1") (Just (Expr "2"))))
            ])
      `shouldBe`
      "SELECT * FROM users LIMIT 1,2"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 Nothing
               )
               [OrderingTerm (Expr "id") Asc]
               Nothing
            ])
      `shouldBe`
      "SELECT * FROM users ORDER BY id ASC"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 Nothing
               )
               [OrderingTerm (Expr "name") Desc]
               Nothing
            ])
      `shouldBe`
      "SELECT * FROM users ORDER BY name DESC"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 (Just $ WhereTerm (Expr "True"))
                 Nothing
               )
               []
               Nothing
            ])
      `shouldBe`
      "SELECT * FROM users WHERE True"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 (Just $ GroupTerm [Expr "name"] Nothing)
               )
               []
               Nothing
            ])
      `shouldBe`
      "SELECT * FROM users GROUP BY name"
    it "" $ (Formatter.format $ SQL [SelectStmt (
               SelectCore [ResultColumn "*"]
                 (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
                 Nothing
                 (Just $ GroupTerm [Expr "name"] (Just $ Expr "True"))
               )
               []
               Nothing
            ])
      `shouldBe`
      "SELECT * FROM users GROUP BY name HAVING True"
