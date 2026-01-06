{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.HeadTail where

import Data.String.Interpolate
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.QuickCheck (ioProperty, forAll)
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Core
import TestLib.Generators


spec :: TopSpec
spec = describe "HeadTailTransformer" $ do
  it "works" $ do
    (ls, _ :: HeadTailTransformer) <- project (["fn main() {"], ["}"]) (listToDoc [[i|println!("hi")|]])
    ls `shouldBe` (listToDoc [
                      "fn main() {"
                      , [i|println!("hi")|]
                      , "}"
                      ])

    -- transformPosition SDParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    -- untransformPosition SDParams ed (Position 1 0) `shouldBe` (Just (Position 1 0))

  describe "QuickCheck" $ introduceQuickCheck $ do
    prop "Does handleDiff for single line changes correctly" $
      forAll (arbitrarySingleLineChange doc) $ \change -> ioProperty $
        testChange @HeadTailTransformer (["fn main() {"], ["}"]) doc change

    -- prop "Does handleDiff for multi line changes correctly" $ do
    --   testChange @HeadTailTransformer (["fn main() {"], ["}"]) doc <$> arbitraryChange doc

  describe "position transformations" $ do
    it "should transform positions with header lines" $ do
      let inputDoc = listToDoc ["line1", "line2", "line3"]
          params = (["header1", "header2"], [])
      (_, ht :: HeadTailTransformer) <- project params inputDoc

      -- Original line 0 should move down by 2 (after 2 header lines)
      transformPosition params ht (Position 0 5) `shouldBe` Just (Position 2 5)
      transformPosition params ht (Position 1 3) `shouldBe` Just (Position 3 3)

    it "should untransform positions with header lines" $ do
      let inputDoc = listToDoc ["line1", "line2", "line3"]
          params = (["header1", "header2"], [])
      (_, ht :: HeadTailTransformer) <- project params inputDoc

      -- Lines 2-4 in output correspond to original lines 0-2
      untransformPosition params ht (Position 2 5) `shouldBe` Just (Position 0 5)
      untransformPosition params ht (Position 3 3) `shouldBe` Just (Position 1 3)

      -- Lines 0-1 are headers, should return Nothing
      untransformPosition params ht (Position 0 0) `shouldBe` Nothing
      untransformPosition params ht (Position 1 0) `shouldBe` Nothing

    it "should handle tail lines correctly" $ do
      let inputDoc = listToDoc ["line1", "line2"]
          params = (["header"], ["tail1", "tail2"])
      (_, ht :: HeadTailTransformer) <- project params inputDoc

      -- Positions beyond original content should return Nothing
      untransformPosition params ht (Position 3 0) `shouldBe` Nothing  -- In tail section
      untransformPosition params ht (Position 4 0) `shouldBe` Nothing  -- In tail section

doc :: Rope
doc = Rope.fromText [i|

println!("Hello world");
eprintln!("Hello error");
format!("Hello {}", "world");

|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
