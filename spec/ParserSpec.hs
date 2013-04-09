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
          SelectStmt (SelectCore [ResultColumn "id", ResultColumn "name"]
            (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) []
          ))
          []
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT id, name FROM users")
       `shouldBe`
       (Right $ SQL [
         SelectStmt (SelectCore [ResultColumn "id", ResultColumn "name"]
           (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) []
         ))
         []
         Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (SelectCore [ResultColumn "*"] (
             JoinSource
               (TableNameSingleSource (TableName Nothing "users") Nothing)
               []
           ))
           []
           Nothing
         ])
    it "" $ (Parser.to_sql "SELECT * FROM users LEFT JOIN emails ON True")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (SelectCore [ResultColumn "*"] (
             JoinSource
               (TableNameSingleSource (TableName Nothing "users") Nothing)
               [LatterSource Outer (TableNameSingleSource (TableName Nothing "emails") Nothing) (OnConstraint "True")]
           ))
           []
           Nothing
         ])
    it "" $ (Parser.to_sql "SELECT * FROM users LIMIT 1")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (SelectCore [ResultColumn "*"]
            (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) []
          ))
          []
          (Just (LimitTerm (Expr "1") Nothing))
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users LEFT JOIN emails ON True LIMIT 1, 3")
       `shouldBe`
       (Right $ SQL [
           SelectStmt (SelectCore [ResultColumn "*"] (
             JoinSource
               (TableNameSingleSource (TableName Nothing "users") Nothing)
               [LatterSource Outer (TableNameSingleSource (TableName Nothing "emails") Nothing) (OnConstraint "True")]
           ))
           []
           (Just (LimitTerm (Expr "1") (Just (Expr "3"))))
         ])
    it "" $ (Parser.to_sql "SELECT * FROM users ORDER BY id")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (SelectCore [ResultColumn "*"]
            (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) []
          ))
          [OrderingTerm (Expr "id") Asc]
          Nothing
       ])
    it "" $ (Parser.to_sql "SELECT * FROM users ORDER BY name DESC")
       `shouldBe`
       (Right $ SQL [
          SelectStmt (SelectCore [ResultColumn "*"]
            (JoinSource (TableNameSingleSource (TableName Nothing "users") Nothing) []
          ))
          [OrderingTerm (Expr "name") Desc]
          Nothing
       ])
