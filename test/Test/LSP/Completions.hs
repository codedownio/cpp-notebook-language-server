{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.Completions where

import Data.Maybe
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import TestLib.Helpers
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers


spec :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => SpecFree ctx m ()
spec = describe "Completions" $ do
    it "provides completions for std namespace in main function" $ do
      let testCode = [__i|\#include <iostream>
                          \#include <vector>
                          int main() {
                              std::
                              return 0;
                          }|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Get completions after "std::"
        completions <- LSP.getCompletions doc (Position 3 9)
        info [i|Got completions: #{completions}|]

        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "iostream"
        insertTexts `listShouldContain` "vector"

    it "provides completions for std namespace at top level" $ do
      let testCode = [__i|\#include <iostream>
                          \#include <vector>
                          std::
                         |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Get completions after "std::"
        completions <- LSP.getCompletions doc (Position 2 5)
        info [i|Got completions: #{completions}|]

        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "iostream"
        insertTexts `listShouldContain` "vector"

    it "provides completions for local variables" $ do
      let testCode = [__i|int main() {
                              int myVariable = 42;
                              double myDouble = 3.14;
                              my
                              return 0;
                          }|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Get completions after "my"
        completions <- LSP.getCompletions doc (Position 3 6)
        info [i|Got completions: #{completions}|]

        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "myVariable"
        insertTexts `listShouldContain` "myDouble"
