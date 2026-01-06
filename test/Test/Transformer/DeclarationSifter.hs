{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.DeclarationSifter where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Transformer
import Test.Sandwich


-- Test input with multi-line declarations that should be preserved
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

-- Expected output with declarations properly sifted and multi-line structures preserved
expectedSiftedOutput :: T.Text
expectedSiftedOutput = T.unlines
  [ "#include <iostream>"
  , "#include <vector>"
  , "using namespace std;"
  , "class MyClass {"
  , "public:"
  , "    int value;"
  , "    void print() { cout << \"Class method\" << endl; }"
  , "};"
  , "int x = 42;"
  , ""
  , "// This is a comment"
  , ""
  , ""
  , "cout << \"Hello from the top!\" << endl;"
  , ""
  , ""
  , "cout << \"After class definition\" << endl;"
  , ""
  ]

spec :: TopSpec
spec = describe "DeclarationSifter" $ do

  it "should preserve complete multi-line declarations" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser") inputDoc
    let outputLines = docToList outputDoc

    -- The key test: class should be preserved as a complete 5-line block
    let classStartIdx = findClassStart outputLines
    case classStartIdx of
      Nothing -> error "Class declaration not found in output"
      Just idx -> do
        let classBlock = take 5 (drop idx outputLines)
            expectedClassBlock = [ "class MyClass {"
                                 , "public:"
                                 , "    int value;"
                                 , "    void print() { cout << \"Class method\" << endl; }"
                                 , "};"
                                 ]
        classBlock `shouldBe` expectedClassBlock

  it "should move declarations to top in correct order" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser") inputDoc
    let outputLines = docToList outputDoc

    -- Check that includes come first, then using, then class, then variables
    (length outputLines >= 4) `shouldBe` True
    T.isPrefixOf "#include" (head outputLines) `shouldBe` True
    T.isPrefixOf "using namespace" (outputLines !! 2) `shouldBe` True
    T.isPrefixOf "class" (outputLines !! 3) `shouldBe` True

  it "should not fragment multi-line structures" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser") inputDoc
    let outputText = T.intercalate "\n" $ docToList outputDoc

    -- Anti-regression test: ensure class is not fragmented
    -- This was the original bug - class lines scattered throughout output
    let classLines = [ "class MyClass {"
                     , "public:"
                     , "    int value;"
                     , "    void print() { cout << \"Class method\" << endl; }"
                     , "};"
                     ]

    -- All class lines should appear consecutively
    containsConsecutiveLines classLines outputText `shouldBe` True

-- Helper function to find where class starts in output
findClassStart :: [T.Text] -> Maybe Int
findClassStart lines = findIndex (T.isPrefixOf "class MyClass") lines
  where
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)

-- Helper function to check if lines appear consecutively
containsConsecutiveLines :: [T.Text] -> T.Text -> Bool
containsConsecutiveLines targetLines text =
  let targetStr = T.intercalate "\n" targetLines
  in targetStr `T.isInfixOf` text
