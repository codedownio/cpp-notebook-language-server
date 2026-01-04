{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.HeadTail where

import Data.String.Interpolate
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Protocol.Types hiding (line)
import Language.LSP.Transformer
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Core
import TestLib.Generators


spec :: TopSpec
spec = describe "HeadTailTransformer" $ do
  it "works" $ do
    let (ls, ht@(HeadTailTransformer {})) = project (["fn main() {"], ["}"]) (listToDoc [[i|println!("hi")|]])
    ls `shouldBe` (listToDoc [
                      "fn main() {"
                      , [i|println!("hi")|]
                      , "}"
                      ])

    -- transformPosition SDParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    -- untransformPosition SDParams ed (Position 1 0) `shouldBe` (Just (Position 1 0))

  describe "QuickCheck" $ introduceQuickCheck $ do
    prop "Does handleDiff for single line changes correctly" $ do
      testChange @HeadTailTransformer (["fn main() {"], ["}"]) doc <$> arbitrarySingleLineChange doc

    -- prop "Does handleDiff for multi line changes correctly" $ do
    --   testChange @HeadTailTransformer (["fn main() {"], ["}"]) doc <$> arbitraryChange doc

doc :: Rope
doc = Rope.fromText [i|

println!("Hello world");
eprintln!("Hello error");
format!("Hello {}", "world");

|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
