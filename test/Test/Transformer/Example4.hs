{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Example4 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.LSP


testCode :: T.Text
testCode = [__i|int x = 42;
                float y = 3.14;
               |]

expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|void __notebook_exec() {
                             int x = 42;
                             float y = 3.14;
                           }
                          |]

spec :: TopSpec
spec = describe "Example4" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms x position" $ do
      transformRoundTripCode testCode (Position 0 4) (Position 1 6)

    it "transforms y position" $ do
      transformRoundTripCode testCode (Position 1 6) (Position 2 8)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
