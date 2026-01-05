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


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
