{-# LANGUAGE DataKinds #-}

import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP

import qualified Test.DocumentReferences

import qualified Test.Transformer.Example1
import qualified Test.Transformer.Example2
import qualified Test.Transformer.Example3

import qualified Test.Hover
import qualified Test.Completions
import qualified Test.DocumentSymbols


spec :: TopSpec
spec = do
  Test.DocumentReferences.spec

  Test.Transformer.Example1.spec
  Test.Transformer.Example2.spec
  Test.Transformer.Example3.spec

  describe "Integration tests" $
    introduceMaybeBubblewrap $
    introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"clangd" "clang-tools" $
    introduceCnls $ do
      Test.Hover.spec
      Test.Completions.spec
      Test.DocumentSymbols.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
