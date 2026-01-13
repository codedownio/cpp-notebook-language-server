{-# LANGUAGE DataKinds #-}

import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP

import qualified Test.Hover
import qualified Test.Pipeline

import qualified Test.Transformer.DeclarationSifter
import qualified Test.LSP.Hover
import qualified Test.LSP.Completions
import qualified Test.LSP.DocumentSymbols


spec :: TopSpec
spec = do
  Test.Hover.spec
  Test.Transformer.DeclarationSifter.spec
  Test.Pipeline.spec

  introduceMaybeBubblewrap $
    introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"clangd" "clang-tools" $
    introduceCnls $ do
      Test.LSP.Hover.spec
      Test.LSP.Completions.spec
      Test.LSP.DocumentSymbols.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
