{-# LANGUAGE OverloadedStrings,  TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseAndFormatCombinationSpec where

import Test.Hspec
import Test.Hspec.QuickCheck ()
import Test.QuickCheck()

import Parser
import Formatter
import Type

import Text.Parsec (ParseError)

prop_format_to_sql :: String -> Bool
prop_format_to_sql sql_string = (sql_string == Formatter.format (getRightValue (Parser.to_sql sql_string)))

getRightValue :: Either ParseError SQL -> SQL
getRightValue (Right x) = x
getRightValue (Left _) = SQL []

spec :: Spec
spec = do
    describe "" $ do
        it "" $ to_sql_and_format "SELECT id,name FROM users"
        it "" $ to_sql_and_format "SELECT id FROM users"
        it "" $ to_sql_and_format "SELECT id FROM users LEFT JOIN emails ON 1"
        it "" $ to_sql_and_format "SELECT id FROM users LIMIT 1"
        it "" $ to_sql_and_format "SELECT id FROM users LIMIT 1,2"
        it "" $ to_sql_and_format "SELECT id FROM users ORDER BY 1 ASC"
        it "" $ to_sql_and_format "SELECT id FROM users ORDER BY 1 DESC"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE 1"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE NOT 1"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE flag IS NULL"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE flag IS NOT NULL"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE NOT flag IS NOT NULL"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE in IN (1, 2, 3)"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE in IN ((1 + 2), (2 + 3) * 4, 3)"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE (123 + 456)"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE CHAR(1, 2, 3)"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE CURRENT_TIME()"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE users.age * 3"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE (1 + users.age * 3)"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE 1 LIKE 2"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE 1 NOT LIKE 2"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE name LIKE \"%Suzuki\""
        it "" $ to_sql_and_format "SELECT id FROM users WHERE name NOT LIKE \"%Suzuki\""
        it "" $ to_sql_and_format "SELECT id FROM users WHERE (users.age + users.age)"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE \"foo\" * db_foo.users.age"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE \"foo\""
        it "" $ to_sql_and_format "SELECT id FROM users WHERE \"f\\\"oo\""
        it "" $ to_sql_and_format "SELECT id FROM users WHERE db_foo.users.email"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE users.email"
        it "" $ to_sql_and_format "SELECT id FROM users WHERE email"
        it "" $ to_sql_and_format "SELECT id FROM users GROUP BY 1"
        it "" $ to_sql_and_format "SELECT id FROM users GROUP BY 1 HAVING 3"
        it "" $ to_sql_and_format "SELECT id,name FROM users; SELECT id FROM groups"
          where
            to_sql_and_format query = (Formatter.format (getRightValue (Parser.to_sql query))) `shouldBe` query
