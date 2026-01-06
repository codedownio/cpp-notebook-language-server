{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.ExecutableWrapper where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.ExecutableWrapper
import Language.LSP.Transformer
import Test.Sandwich

-- Test input with mixed declarations and executable statements (raw input)
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

-- Test input after DeclarationSifter (properly sifted - this is what ExecutableWrapper should receive)
siftedTestCode :: T.Text
siftedTestCode = [__i|
  \#include <iostream>
  using namespace std;
  class MyClass {};
  void func() {}
  int x = 42;

  cout << "hello" << endl;

  cout << "after" << endl;
  |]

spec :: TopSpec
spec = describe "ExecutableWrapper" $ do

  it "should work with already-sifted input" $ do
    let inputDoc = listToDoc (T.splitOn "\n" siftedTestCode)  -- Use sifted input
    (outputDoc, wrapper :: ExecutableWrapper) <- project (ExecutableWrapperParams "__notebook_exec" "minimal-parser") inputDoc
    let outputLines = docToList outputDoc
        outputText = T.intercalate "\n" outputLines


    -- Check that it created a wrapper function
    hasWrapper wrapper `shouldBe` True
    
    -- Check that executable statements are wrapped in function
    T.isInfixOf "void __notebook_exec() {" outputText `shouldBe` True
    T.isInfixOf "  cout << \"hello\" << endl;" outputText `shouldBe` True
    T.isInfixOf "  cout << \"after\" << endl;" outputText `shouldBe` True

  it "should preserve declarations at the top when using sifted input" $ do
    let inputDoc = listToDoc (T.splitOn "\n" siftedTestCode)  -- Use sifted input
    (outputDoc, _ :: ExecutableWrapper) <- project (ExecutableWrapperParams "__notebook_exec" "minimal-parser") inputDoc
    let outputLines = docToList outputDoc

    -- Includes should still be at the top  
    (length outputLines > 0) `shouldBe` True
    case outputLines of
      [] -> error "No output lines"
      (firstLine:_) -> T.isPrefixOf "#include" firstLine `shouldBe` True

  it "should handle input with no executable statements" $ do
    let declarationsOnly = [__i|
          \#include <iostream>
          using namespace std;
          class MyClass {};
          int x = 42;
          |]
        inputDoc = listToDoc (T.splitOn "\n" declarationsOnly)
    (_, wrapper :: ExecutableWrapper) <- project (ExecutableWrapperParams "__notebook_exec" "minimal-parser") inputDoc
    
    -- Should not create wrapper if no executable statements
    hasWrapper wrapper `shouldBe` False

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec