{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Example3 where

import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.LSP


testCode :: T.Text
testCode = [__i|
  \#include <iostream>
  using namespace std;
  cout << "hello" << endl;
  int x = 42;
  class MyClass {};
  void func() {}
  cout << "after" << endl;
  |]

expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|
  \#include <iostream>
  using namespace std;
  class MyClass {};
  void func() {}
  void __notebook_exec() {
    cout << "hello" << endl;
    int x = 42;
    cout << "after" << endl;
  }|]

spec :: TopSpec
spec = describe "Example3" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, sifter :: DeclarationSifter, eitherErr) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    eitherErr `shouldBe` Right ()
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

    forward sifter `shouldBe` V.fromList [0, 1, 5, 6, 2, 3, 7]
    inverse sifter `shouldBe` V.fromList [0, 1, 4, 5, -1, 2, 3, 6, -1]
    wrapperBodyStart sifter `shouldBe` 5
    wrapperBodyEnd sifter `shouldBe` 7

  describe "position transformations" $ do
    it "transforms #include (sifted, stays at line 0)" $ do
      transformRoundTripCode testCode (Position 0 0) (Position 0 0)

    it "transforms cout (executable, wrapped with indent)" $ do
      transformRoundTripCode testCode (Position 2 0) (Position 5 2)

    it "transforms second cout" $ do
      transformRoundTripCode testCode (Position 6 0) (Position 7 2)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
