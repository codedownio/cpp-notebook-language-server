{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.DeclarationSifter where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
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
expectedSiftedOutput = [__i|
  \#include <iostream>
  \#include <vector>
  using namespace std;
  class MyClass {
  public:
      int value;
      void print() { cout << "Class method" << endl; }
  };
  int x = 42;

  // This is a comment


  cout << "Hello from the top!" << endl;


  cout << "After class definition" << endl;

  |]

spec :: TopSpec
spec = describe "DeclarationSifter" $ do

  it "should preserve complete multi-line declarations" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" False "") inputDoc
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
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" False "") inputDoc
    let outputLines = docToList outputDoc

    -- Check that includes come first, then using, then class, then variables
    (length outputLines >= 4) `shouldBe` True
    T.isPrefixOf "#include" (head outputLines) `shouldBe` True
    T.isPrefixOf "using namespace" (outputLines !! 2) `shouldBe` True
    T.isPrefixOf "class" (outputLines !! 3) `shouldBe` True

  it "should not fragment multi-line structures" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" False "") inputDoc
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

  it "should wrap executable statements when requested" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" True "__notebook_exec") inputDoc
    let outputText = T.intercalate "\n" $ docToList outputDoc

    -- Check that wrapper was created
    hasExecutableWrapper sifter `shouldBe` True

    -- Check that executable statements are wrapped in function
    T.isInfixOf "void __notebook_exec() {" outputText `shouldBe` True
    T.isInfixOf "  cout << \"Hello from the top!\" << endl;" outputText `shouldBe` True
    T.isInfixOf "  cout << \"After class definition\" << endl;" outputText `shouldBe` True
    T.isInfixOf "}" outputText `shouldBe` True

  describe "position transformations" $ do
    it "should transform positions correctly after sifting" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" False "") inputDoc

      -- Original line 7 (cout << "Hello from the top!" << endl;) moves to later position
      -- Original line 11 (class MyClass {) moves earlier
      case transformPosition (DeclarationSifterParams "minimal-parser" False "") sifter (Position 7 5) of
        Nothing -> error "Position transformation failed"
        Just (Position newLine _) -> (newLine > 7) `shouldBe` True  -- Should move later

      case transformPosition (DeclarationSifterParams "minimal-parser" False "") sifter (Position 11 2) of
        Nothing -> error "Position transformation failed"
        Just (Position newLine _) -> (newLine < 11) `shouldBe` True  -- Should move earlier

    it "should untransform positions correctly" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" False "") inputDoc

      -- Test round-trip: transform then untransform should give original position
      let originalPos = Position 5 10
      case transformPosition (DeclarationSifterParams "minimal-parser" False "") sifter originalPos of
        Nothing -> error "Transform failed"
        Just transformedPos ->
          case untransformPosition (DeclarationSifterParams "minimal-parser" False "") sifter transformedPos of
            Nothing -> error "Untransform failed"
            Just finalPos -> finalPos `shouldBe` originalPos

    it "should handle wrapper positions when wrapping is enabled" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (outputDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" True "__notebook_exec") inputDoc
      let outputLines = docToList outputDoc

      -- Find where the wrapper function starts
      case findIndex (T.isInfixOf "void __notebook_exec()") outputLines of
        Nothing -> error "Wrapper function not found"
        Just wrapperIdx -> do
          -- Position inside wrapper should be adjusted
          let posInWrapper = Position (fromIntegral wrapperIdx + 1) 2
          case untransformPosition (DeclarationSifterParams "minimal-parser" True "__notebook_exec") sifter posInWrapper of
            Nothing -> error "Untransform of wrapper position failed"
            Just _ -> return ()  -- Just verify it doesn't fail
  where
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)

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
