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
    it "" $ (Parser.to_sql "SELECT id,name FROM users")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "id", ResultColumn "name"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              Nothing
              Nothing
          )
          []
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT id, name FROM users")
       `shouldBe`
       (Right $ SQL [
         SelectStmt (
           SelectCore [ResultColumn "id", ResultColumn "name"]
             (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
             Nothing
             Nothing
         )
         []
         Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (
             SelectCore [ResultColumn "*"]
               (JoinSource
                 (TableNameSingleSource (TableName Nothing "users") Nothing)
                 []
               )
               Nothing
               Nothing
           )
           []
           Nothing
         ])
    it "" $ (Parser.to_sql "SELECT * FROM users LEFT JOIN emails ON True")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (
             SelectCore [ResultColumn "*"] (
               JoinSource
                 (TableNameSingleSource (TableName Nothing "users") Nothing)
                 [LatterSource Outer (TableNameSingleSource (TableName Nothing "emails") Nothing) (OnConstraint "True")]
               )
               Nothing
               Nothing
           )
           []
           Nothing
         ])
    it "" $ (Parser.to_sql "SELECT * FROM users LIMIT 1")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "*"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              Nothing
              Nothing
          )
          []
          (Just (LimitTerm (Expr "1") Nothing))
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users LEFT JOIN emails ON True LIMIT 1, 3")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (
             SelectCore [ResultColumn "*"] (
               JoinSource
                 (TableNameSingleSource (TableName Nothing "users") Nothing)
                 [LatterSource Outer (TableNameSingleSource (TableName Nothing "emails") Nothing) (OnConstraint "True")]
               )
               Nothing
               Nothing
           )
           []
           (Just (LimitTerm (Expr "1") (Just (Expr "3"))))
         ])
    it "" $ (Parser.to_sql "SELECT * FROM users ORDER BY id")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "*"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              Nothing
              Nothing
          )
          [OrderingTerm (Expr "id") Asc]
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users ORDER BY name DESC")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "*"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              Nothing
              Nothing
          )
          [OrderingTerm (Expr "name") Desc]
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users WHERE True")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "*"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              (Just $ WhereTerm (Expr "True"))
              Nothing
          )
          []
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users GROUP BY name")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "*"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              Nothing
              (Just $ GroupTerm [Expr "name"] Nothing)
          )
          []
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users GROUP BY name HAVING True")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (
            SelectCore [ResultColumn "*"]
              (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
              Nothing
              (Just $ GroupTerm [Expr "name"] (Just $ Expr "True"))
          )
          []
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT id,name FROM users; SELECT * FROM groups")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (
             SelectCore [ResultColumn "id", ResultColumn "name"]
               (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) [])
               Nothing
               Nothing
           )
           []
           Nothing,
           SelectStmt (
             SelectCore [ResultColumn "*"]
               (JoinSource (TableNameSingleSource (TableName Nothing "groups") Nothing) [])
               Nothing
               Nothing
           )
           []
           Nothing
         ])
