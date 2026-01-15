{-# LANGUAGE DataKinds #-}

import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP

import qualified Test.DocumentReferences

import qualified Test.Transformer.DeclarationSifter
import qualified Test.Transformer.DeclarationSifter2

import qualified Test.Pipeline

import qualified Test.LSP.Hover
import qualified Test.LSP.Completions
import qualified Test.LSP.DocumentSymbols


spec :: TopSpec
spec = do
  Test.DocumentReferences.spec

  Test.Transformer.DeclarationSifter.spec
  Test.Transformer.DeclarationSifter2.spec

  Test.Pipeline.spec

  describe "Integration tests" $
    introduceMaybeBubblewrap $
    introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"clangd" "clang-tools" $
    introduceCnls $ do
      Test.LSP.Hover.spec
      Test.LSP.Completions.spec
      Test.LSP.DocumentSymbols.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
