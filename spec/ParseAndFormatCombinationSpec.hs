{-# LANGUAGE OverloadedStrings,  TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseAndFormatCombinationSpec where

import Test.Hspec
import Test.Hspec.QuickCheck ()
import Test.QuickCheck()

import Parser
import Formatter
import Type
import Arbitrary

import Text.Parsec (ParseError)

-- instance Eq ParseError where
--   _ == __ = False

--instance Testable SQL where
--  property (SQL qs) = result $ nothing { ok = Nothing }

-- 現状cabal testでは動作しないので以下の手順でテストする.
--  1. $ ghci -isrc -ispec
--  2. Prelude> :m Test.QuickCheck
--  3. Prelude Test.QuickCheck> :load "spec/ParseAndFormatCombinationSpec.hs"
--  4. quickCheck prop_to_sql_format

prop_to_sql_format :: SQL -> Bool
prop_to_sql_format sql_ = (sql_ == getRightValue (Parser.to_sql (Formatter.format sql_)))

prop_format_to_sql :: String -> Bool
prop_format_to_sql sql_string = (sql_string == Formatter.format (getRightValue (Parser.to_sql sql_string)))

getRightValue :: Either ParseError SQL -> SQL
getRightValue (Right x) = x
getRightValue (Left _) = SQL []

spec :: Spec
spec = do
    describe "" $ do
        it "" $ prop_format_to_sql "SELECT id,name FROM users" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users LEFT JOIN emails ON 1" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users LIMIT 1" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users LIMIT 1,2" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users ORDER BY 1 ASC" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users ORDER BY 1 DESC" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users WHERE 1" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users GROUP BY 1" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT * FROM users GROUP BY 1 HAVING 3" `shouldBe` True
        it "" $ prop_format_to_sql "SELECT id,name FROM users; SELECT * FROM groups" `shouldBe` True
