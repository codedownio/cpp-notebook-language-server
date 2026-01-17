{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Example1 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich


testCode :: T.Text
testCode = [__i|
  \#include <iostream>
  \#include <vector>

  // This is a comment

  using namespace std;

  cout << "Hello from the top!" << endl;

  int x = 42;

  class MyClass {
  public:
      int value;
      void print() { cout << "Class method" << endl; }
  };

  cout << "After class definition" << endl;
  |]

expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|
  \#include <iostream>
  \#include <vector>
  using namespace std;
  class MyClass {
  public:
      int value;
      void print() { cout << "Class method" << endl; }
  };
  int x = 42;

  void __notebook_exec() {
    // This is a comment


    cout << "Hello from the top!" << endl;



    cout << "After class definition" << endl;
  }|]

spec :: TopSpec
spec = describe "Example1" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms (7, 5) to (15, 7)" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ transformPosition params sifter (Position 7 5)
      pos `shouldBe` Position 15 7

    it "untransforms (15, 7) to (7, 5)" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 15 7)
      pos `shouldBe` Position 7 5

    it "clamps column to 0 when untransforming from indentation area" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 15 1)
      pos `shouldBe` Position 7 0
