module ParserSpec where

import Test.Hspec
import qualified Parser
import Type

spec :: Spec
spec = do
  describe "" $ do
    it "" $ (Parser.parse "str") `shouldBe` (SQL "")
