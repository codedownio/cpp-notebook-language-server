{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.Completions where

import Control.Lens ((^.))
import Control.Monad.IO.Unlift
import Control.Monad.Catch (MonadMask)
import Control.Applicative
import Data.Maybe
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (hover)
import qualified Data.List as L
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Data.Text as T
import qualified Language.LSP.Test.Helpers as Helpers


spec :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => SpecFree ctx m ()
spec = describe "C++ LSP Completions Tests" $ do
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

shouldContainAny :: MonadIO m => [T.Text] -> [T.Text] -> m ()
shouldContainAny haystack needles =
  if any (`elem` haystack) needles
    then return ()
    else expectationFailure [i|Expected #{haystack} to contain at least one of #{needles}|]

shouldBeAtLeast :: MonadIO m => Int -> Int -> m ()
shouldBeAtLeast actual expected =
  if actual >= expected
    then return ()
    else expectationFailure [i|Expected at least #{expected} but got #{actual}|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]

-- main :: IO ()
-- main = runSandwichWithCommandLineArgs defaultOptions spec
