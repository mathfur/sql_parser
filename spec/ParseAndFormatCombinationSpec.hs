{-# LANGUAGE OverloadedStrings,  TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseAndFormatCombinationSpec where

import Test.Hspec
import Test.Hspec.QuickCheck ()
import Test.QuickCheck()
import Text.Parsec (ParseError)

import Parser
import Formatter
import Type
import ProcessSql

prop_format_to_sql :: String -> Bool
prop_format_to_sql sql_string = (sql_string == Formatter.format (getRightValue (Parser.to_sql sql_string)))

getRightValue :: Either ParseError SQL -> SQL
getRightValue (Right x) = x
getRightValue (Left _) = SQL []

spec :: Spec
spec = do
    describe "Main parse functionality" $ do
        it "" $ assert_of_to_sql_and_format "SELECT * FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT * FROM users UNION ALL SELECT id FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT * FROM users UNION SELECT id FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT users.id FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT id,name FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT 1 FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT id AS foo,name AS bar FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT id AS foo FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT count(*) FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT count(id) FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT count(id) AS count FROM users"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users LEFT JOIN emails ON 1"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users LIMIT 1"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users LIMIT 1,2"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users ORDER BY 1 ASC"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users ORDER BY 1 DESC"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE 1"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE NOT 1"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE flag IS NULL"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE flag IS NOT NULL"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE NOT flag IS NOT NULL"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE in IN (1, 2, 3)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE in IN ((1 + 2), (2 + 3) * 4, 3)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE (123 + 456)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE CHAR(1, 2, 3)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE CURRENT_TIME()"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE users.age * 3 = 30"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE (1 + users.age * 3)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE 1 LIKE 2"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE (1 + 2) = 3"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE 1 NOT LIKE 2"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE name LIKE \"%Suzuki\""
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE name NOT LIKE \"%Suzuki\""
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE age BETWEEN 10 AND 20"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE True AND False"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE (True OR False)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE age NOT BETWEEN 10 AND 20"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE (users.age + users.age)"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE \"foo\" * db_foo.users.age"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE \"foo\""
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE \"f\\\"oo\""
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE db_foo.users.email"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE users.email"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users WHERE email"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users GROUP BY 1"
        it "" $ assert_of_to_sql_and_format "SELECT id FROM users GROUP BY 1 HAVING 3"
        it "" $ assert_of_to_sql_and_format "SELECT id,name FROM users; SELECT id FROM groups"
        it "" $ (to_sql_and_format "SELECT id FROM `users`") `shouldBe` "SELECT id FROM users"
        it "" $ (to_sql_and_format "SELECT id FROM `users` WHERE (`users`.`age` = 30)") `shouldBe` "SELECT id FROM users WHERE users.age = 30"
        it "" $ (to_sql_and_format "SELECT `users`.`id` FROM `users`") `shouldBe` "SELECT users.id FROM users"
        it "" $ (to_sql_and_format "SELECT id FROM db_name.`users`") `shouldBe` "SELECT id FROM db_name.users"
    describe "" $ do
        it "" $ (get_all_tables_from_sql "SELECT * FROM users OUTER JOIN companies ON 1") `shouldBe` ["users", "companies"]
            where
                assert_of_to_sql_and_format query = (to_sql_and_format query) `shouldBe` query
                to_sql_and_format  = Formatter.format . getRightValue . Parser.to_sql
                get_all_tables_from_sql = get_all_tables . getRightValue . Parser.to_sql
