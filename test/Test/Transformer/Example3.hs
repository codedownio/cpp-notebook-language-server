{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Example3 where

import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich


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
  int x = 42;
  void __notebook_exec() {
    cout << "hello" << endl;
    cout << "after" << endl;
  }|]

spec :: TopSpec
spec = describe "Example3" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

    -- forward[origLine] = outputLine
    forward sifter `shouldBe` V.fromList [0, 1, 6, 4, 2, 3, 7]
    -- inverse[outputLine] = origLine (-1 for synthetic header/closing)
    inverse sifter `shouldBe` V.fromList [0, 1, 4, 5, 3, -1, 2, 6, -1]
    -- wrapper body is lines 6-7
    wrapperBodyStart sifter `shouldBe` 6
    wrapperBodyEnd sifter `shouldBe` 7

  describe "position transformations" $ do
    it "transforms #include (sifted, stays at line 0)" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- #include is at line 0 in input, stays at line 0 in output
      Just pos <- return $ transformPosition params sifter (Position 0 0)
      pos `shouldBe` Position 0 0

    it "untransforms #include" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 0 0)
      pos `shouldBe` Position 0 0

    it "transforms var (sifted but moved)" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- int x is at line 3 in input, should move to line 4 in output (include, using, class, func, VAR)
      Just pos <- return $ transformPosition params sifter (Position 3 0)
      pos `shouldBe` Position 4 0

    it "untransforms var" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 4 0)
      pos `shouldBe` Position 3 0

    it "transforms cout (executable, wrapped with indent)" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- First cout is at line 2 col 0 in input
      -- In output: 5 sifted lines (0-4), header at 5, body at 6-7, close at 8
      -- The cout should be at line 6 with +2 column indent
      Just pos <- return $ transformPosition params sifter (Position 2 0)
      pos `shouldBe` Position 6 2

    it "untransforms cout" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 6 2)
      pos `shouldBe` Position 2 0

    it "transforms second cout" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      -- Second cout is at line 6 col 0 in input, should be at line 7 col 2 in output
      Just pos <- return $ transformPosition params sifter (Position 6 0)
      pos `shouldBe` Position 7 2

    it "untransforms second cout" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 7 2)
      pos `shouldBe` Position 6 0

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
