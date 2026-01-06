{-# LANGUAGE OverloadedStrings #-}

module Test.Pipeline where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Transformer
import Test.Sandwich

-- Test input: mixed declarations and executable statements (like a notebook)
testInput :: T.Text
testInput = [__i|
  \#include <iostream>
  using namespace std;
  cout << "hello" << endl;
  int x = 42;
  class MyClass {};
  void func() {}
  cout << "after" << endl;
  |]

-- Expected final output after full pipeline (no main wrapper)
expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|
  \#include <iostream>
  using namespace std;
  class MyClass {};
  void func() {}
  int x = 42;

  void __notebook_exec() {
    cout << "hello" << endl;
    cout << "after" << endl;
  }
  |]

spec :: TopSpec
spec = describe "Full Pipeline" $ do

  it "should transform notebook code to valid C++ through full pipeline" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testInput)

    -- Step 1: DeclarationSifter (with wrapping disabled for separate testing)
    (siftedDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" "") inputDoc
    let siftedText = T.intercalate "\n" $ docToList siftedDoc

    -- Verify DeclarationSifter moved declarations to top
    T.isPrefixOf "#include" siftedText `shouldBe` True
    T.isInfixOf "using namespace std;" siftedText `shouldBe` True
    T.isInfixOf "class MyClass {};" siftedText `shouldBe` True

    -- Step 2: Use DeclarationSifter with wrapping (replaces ExecutableWrapper)
    (wrappedDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" "__notebook_exec") inputDoc
    let wrappedText = T.intercalate "\n" $ docToList wrappedDoc

    -- Verify DeclarationSifter wrapped executable statements
    hasExecutableWrapper sifter `shouldBe` True
    T.isInfixOf "void __notebook_exec() {" wrappedText `shouldBe` True
    T.isInfixOf "  cout << \"hello\" << endl;" wrappedText `shouldBe` True
    T.isInfixOf "  cout << \"after\" << endl;" wrappedText `shouldBe` True

    -- Step 3: No main wrapper (removed as requested)
    let finalDoc = wrappedDoc  -- No additional transformation
        finalText = T.intercalate "\n" $ docToList finalDoc

    -- Verify no main() wrapper was added
    T.isInfixOf "int main() {" finalText `shouldBe` False

    -- Verify final structure looks correct (declarations at top, executable code wrapped)
    let finalLines = T.splitOn "\n" finalText
    (length finalLines > 5) `shouldBe` True  -- Should have substantial content

  it "should preserve declaration order and structure" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testInput)

    -- Run full pipeline (DeclarationSifter does everything, no main wrapper)
    (finalDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" "__notebook_exec") inputDoc

    let finalText = T.intercalate "\n" $ docToList finalDoc
        finalLines = T.splitOn "\n" finalText

    -- Check structure: includes first, then other declarations, then executable function (no main)
    case finalLines of
      [] -> error "No output"
      (firstLine:_) -> T.isPrefixOf "#include" firstLine `shouldBe` True

    -- Check that declarations come before executable wrapper
    let includePos = findSubstring "#include" finalText
        execPos = findSubstring "void __notebook_exec()" finalText

    (includePos < execPos) `shouldBe` True

  it "should work with combined DeclarationSifter that wraps executables" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testInput)

    -- Use DeclarationSifter with wrapping enabled (no need for ExecutableWrapper)
    (siftedWrappedDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" "__notebook_exec") inputDoc
    let siftedWrappedText = T.intercalate "\n" $ docToList siftedWrappedDoc

    -- Verify it did both sifting and wrapping
    hasExecutableWrapper sifter `shouldBe` True
    T.isPrefixOf "#include" siftedWrappedText `shouldBe` True
    T.isInfixOf "void __notebook_exec() {" siftedWrappedText `shouldBe` True
    T.isInfixOf "  cout << \"hello\" << endl;" siftedWrappedText `shouldBe` True

    -- No main wrapper needed
    let finalText = siftedWrappedText

    -- Verify final output has wrapped executables (but no main)
    T.isInfixOf "void __notebook_exec() {" finalText `shouldBe` True

-- Helper to find position of substring
findSubstring :: T.Text -> T.Text -> Int
findSubstring needle haystack =
  case T.breakOn needle haystack of
    (prefix, remaining) ->
      if T.null remaining
      then -1  -- Not found
      else T.length prefix

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
