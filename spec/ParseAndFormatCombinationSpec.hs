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

getRightValue :: Either ParseError SQL -> SQL
getRightValue (Right x) = x
getRightValue (Left _) = SQL []

spec :: Spec
spec = do
 describe "" $ do
         it "" $ True `shouldBe` True
--   prop "" prop_to_sql_format
