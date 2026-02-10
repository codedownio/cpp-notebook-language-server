{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Example1 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.LSP


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
  void __notebook_exec() {

    // This is a comment


    cout << "Hello from the top!" << endl;

    int x = 42;


    cout << "After class definition" << endl;
  }|]

spec :: TopSpec
spec = describe "Example1" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms stream operator" $ do
      info [i|expectedFinalOutput: #{show expectedFinalOutput}|]
      transformRoundTripCode testCode (Position 7 5) (Position 13 7)

    it "clamps column to 0 when untransforming from indentation area" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 13 1)
      pos `shouldBe` Position 7 0

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
