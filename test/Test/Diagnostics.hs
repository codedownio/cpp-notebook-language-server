{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Diagnostics where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers


spec :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => SpecFree ctx m ()
spec = describe "Diagnostics" $ do
  it "provides diagnostics for local variables" $ do
    let testCode = [__i|\n\n\nint myVariable = 42;
                        double myDouble = 3.14;
                        my|]

    doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
      _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

      (Helpers.getDiagnosticRanges' <$>) (LSP.waitForDiagnosticsSource "clang")
        >>= (`shouldBe` [(Range (Position 5 0) (Position 5 2), Just (InR "undeclared_var_use"), "Use of undeclared identifier 'my'")])
