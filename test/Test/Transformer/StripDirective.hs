{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.StripDirective where

import Language.LSP.Notebook.StripDirective
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich


spec :: TopSpec
spec = describe "StripDirective" $ do
  it "strips out :dep directives" $ do
    (ls, ed@(StripDirective _ affectedLines)) <- project (SDParams True) (listToDoc ["let foo = 42", ":dep rand"])
    ls `shouldBe` (listToDoc ["let foo = 42", ""])
    affectedLines `shouldBe` [1]

    transformPosition (SDParams True) ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    untransformPosition (SDParams True) ed (Position 1 0) `shouldBe` (Just (Position 1 0))

  describe "position transformations" $ do
    it "should handle positions when no lines are stripped" $ do
      (_, sd :: StripDirective) <- project (SDParams True) (listToDoc ["let x = 1", "let y = 2"])

      -- No directives, so positions should be unchanged
      transformPosition (SDParams True) sd (Position 0 5) `shouldBe` Just (Position 0 5)
      transformPosition (SDParams True) sd (Position 1 3) `shouldBe` Just (Position 1 3)

      untransformPosition (SDParams True) sd (Position 0 5) `shouldBe` Just (Position 0 5)
      untransformPosition (SDParams True) sd (Position 1 3) `shouldBe` Just (Position 1 3)

    it "should transform positions on stripped directive lines" $ do
      (_, sd :: StripDirective) <- project (SDParams True) (listToDoc ["let x = 1", ":dep some-lib", "let y = 2"])

      -- Line 1 was stripped, positions on it should move to column 0
      transformPosition (SDParams True) sd (Position 1 5) `shouldBe` Just (Position 1 0)
      transformPosition (SDParams True) sd (Position 1 10) `shouldBe` Just (Position 1 0)

      -- Other lines unchanged
      transformPosition (SDParams True) sd (Position 0 5) `shouldBe` Just (Position 0 5)
      transformPosition (SDParams True) sd (Position 2 3) `shouldBe` Just (Position 2 3)

    it "should handle multiple directive lines" $ do
      (outputDoc, sd :: StripDirective) <- project (SDParams True) (listToDoc [":dep lib1", "code", ":dep lib2", "more code"])
      let outputLines = docToList outputDoc

      -- Lines 0 and 2 should be empty (stripped)
      (outputLines !! 0) `shouldBe` ""
      (outputLines !! 2) `shouldBe` ""

      -- Positions on stripped lines go to column 0
      transformPosition (SDParams True) sd (Position 0 5) `shouldBe` Just (Position 0 0)
      transformPosition (SDParams True) sd (Position 2 8) `shouldBe` Just (Position 2 0)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
