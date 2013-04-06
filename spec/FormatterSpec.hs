module FormatterSpec where

import Test.Hspec
import qualified Formatter
import Type

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Formatter.format (SQL "")) `shouldBe` "sql"
