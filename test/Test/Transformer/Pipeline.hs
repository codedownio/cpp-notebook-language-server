{-# LANGUAGE OverloadedStrings #-}

module Test.Transformer.Pipeline where

import Data.String.Interpolate
import qualified Data.Text as T
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
spec = describe "Pipeline" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
    -- Verify sifted indices are in OUTPUT order: include[0], using[1], class[4], func[5], var[3]
    show sifter `shouldContain` "[0,1,4,5,3]"
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

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
      -- In output: 5 sifted lines (0-4), then wrapper at line 5-9
      -- The cout should be at line 7 with +2 column indent
      Just pos <- return $ transformPosition params sifter (Position 2 0)
      pos `shouldBe` Position 7 2

    it "untransforms cout" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
      let params = DeclarationSifterParams "cling-parser" "__notebook_exec"

      Just pos <- return $ untransformPosition params sifter (Position 7 2)
      pos `shouldBe` Position 2 0

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
