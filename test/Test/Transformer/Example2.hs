{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Example2 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich


testCode :: T.Text
testCode = [__i|\#include <cmath>
                double result = sqrt(16.0)
               |]

expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|\#include <cmath>
                           void __notebook_exec() {
                             double result = sqrt(16.0)
                           }
                          |]

spec :: TopSpec
spec = describe "Example2" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms sqrt" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- sqrt is at position (1, 16) in input, should be at (2, 18) in output
      -- Line 1 -> line 2 (after header), column 16 + 2 = 18
      Just pos <- return $ transformPosition params sifter (Position 1 16)
      pos `shouldBe` (Position 2 18)

    it "untransforms sqrt" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 2 18)
      pos `shouldBe` Position 1 16
