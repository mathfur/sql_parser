module Arbitrary where

import Test.QuickCheck (elements, oneof, choose, listOf, Gen)
import Test.QuickCheck.Arbitrary
import Type

import Control.Applicative

instance Arbitrary SQL where
  arbitrary = SQL <$> arbitrary

instance Arbitrary SelectStmt where
  arbitrary = SelectStmt <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary OrderingTerm where
  arbitrary = OrderingTerm <$> arbitrary <*> arbitrary

instance Arbitrary Order where
  arbitrary = elements [Asc, Desc]

instance Arbitrary LimitTerm where
  arbitrary = LimitTerm <$> arbitrary <*> arbitrary

instance Arbitrary SelectCore where
  arbitrary = SelectCore <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GroupTerm where
  arbitrary = GroupTerm <$> arbitrary <*> arbitrary

instance Arbitrary JoinSource where
  arbitrary = JoinSource <$> arbitrary <*> arbitrary

-----------------------------------------
instance Arbitrary SingleSource where
  arbitrary = oneof [JoinSingleSource <$> arbitrary, TableNameSingleSource <$> arbitrary <*> arbitrary]

instance Arbitrary LatterSource where
  arbitrary = LatterSource <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary WhereTerm where
  arbitrary = WhereTerm <$> arbitrary

instance Arbitrary ResultColumn where
  arbitrary = do
    cs <- listOf $ choose ('a', 'z')
    return $ ResultColumn cs

instance Arbitrary JoinOp where
  arbitrary = elements [Inner, Outer]

instance Arbitrary JoinConstraint where
  arbitrary = oneof [OnConstraint <$> arbitrary, UsingConstraint <$> arbitrary]

instance Arbitrary ColumnName where
  arbitrary = ColumnName <$> letters

letters :: Gen String
letters = listOf $ choose ('a', 'z')

instance Arbitrary TableAlias where
  arbitrary = TableAlias <$> letters

instance Arbitrary TableName where
  arbitrary = TableName <$> (oneof [Just <$> letters, pure Nothing]) <*> letters

instance Arbitrary Expr where
  arbitrary = elements [Expr "True"] -- TODO
