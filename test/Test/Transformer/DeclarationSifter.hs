{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.DeclarationSifter where

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
spec = describe "DeclarationSifter" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "should transform positions correctly after sifting" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "") inputDoc

      -- Original line 7 (cout << "Hello from the top!" << endl;) moves to later position
      -- Original line 11 (class MyClass {) moves earlier
      case transformPosition (DeclarationSifterParams "cling-parser" "") sifter (Position 7 5) of
        Nothing -> error "Position transformation failed"
        Just (Position newLine _) -> (newLine > 7) `shouldBe` True  -- Should move later

      case transformPosition (DeclarationSifterParams "cling-parser" "") sifter (Position 11 2) of
        Nothing -> error "Position transformation failed"
        Just (Position newLine _) -> (newLine < 11) `shouldBe` True  -- Should move earlier

    it "should untransform positions correctly" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "") inputDoc

      -- Test round-trip: transform then untransform should give original position
      let originalPos = Position 5 10
      case transformPosition (DeclarationSifterParams "cling-parser" "") sifter originalPos of
        Nothing -> error "Transform failed"
        Just transformedPos ->
          case untransformPosition (DeclarationSifterParams "cling-parser" "") sifter transformedPos of
            Nothing -> error "Untransform failed"
            Just finalPos -> finalPos `shouldBe` originalPos

    it "should handle wrapper positions when wrapping is enabled" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (outputDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let outputLines = docToList outputDoc

      -- Find where the wrapper function starts
      case findIndex (T.isInfixOf "void __notebook_exec()") outputLines of
        Nothing -> error "Wrapper function not found"
        Just wrapperIdx -> do
          -- Position inside wrapper should be adjusted
          let posInWrapper = Position (fromIntegral wrapperIdx + 1) 2
          case untransformPosition (DeclarationSifterParams "cling-parser" "__notebook_exec") sifter posInWrapper of
            Nothing -> error "Untransform of wrapper position failed"
            Just _ -> return ()  -- Just verify it doesn't fail

    it "transformPosition should add 2 to column for wrapped lines" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- Line 7 in testCode is "cout << ..." which gets wrapped
      let Position _ origCol = Position 7 5
      case transformPosition params sifter (Position 7 5) of
        Nothing -> error "Transform failed"
        Just (Position _ newCol) -> newCol `shouldBe` (origCol + 2)

    it "untransformPosition should subtract 2 from column for wrapped lines" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (outputDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let outputLines = docToList outputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- Find indented line in wrapper, test column subtraction and clamping
      case findIndex (T.isPrefixOf "  cout") outputLines of
        Nothing -> error "Indented cout line not found"
        Just idx -> do
          let wrappedLine = fromIntegral idx
          -- Column 10 -> 8
          case untransformPosition params sifter (Position wrappedLine 10) of
            Nothing -> error "Untransform failed"
            Just (Position _ c) -> c `shouldBe` 8
          -- Column 1 -> 0 (clamped)
          case untransformPosition params sifter (Position wrappedLine 1) of
            Nothing -> error "Untransform failed"
            Just (Position _ c) -> c `shouldBe` 0
  where
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)
