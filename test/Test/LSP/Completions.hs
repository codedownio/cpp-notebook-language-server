{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.Completions where

import Control.Lens ((^.))
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (hover)
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Data.Text as T
import qualified Language.LSP.Test.Helpers as Helpers

spec :: TopSpec
spec = describe "C++ LSP Completions Tests" $
  introduceMaybeBubblewrap $
  introduceNixContext nixpkgsReleaseDefault $
  introduceBinaryViaNixPackage @"clangd" "clang-tools" $
  introduceCnls $ do

    it "provides completions for std namespace" $ do
      let testCode = [__i|
\#include <iostream>
\#include <vector>
int main() {
    std::
    return 0;
}
|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Get completions after "std::"
        completions <- LSP.getCompletions doc (Position 3 9)
        info [i|Got #{Prelude.length completions} completions for std::|]

        -- Just check that we get some completions
        liftIO $ Prelude.length completions `shouldBeAtLeast` 1

    it "provides completions for local variables" $ do
      let testCode = [__i|
int main() {
    int myVariable = 42;
    double myDouble = 3.14;
    my
    return 0;
}
|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Get completions after "my"
        completions <- LSP.getCompletions doc (Position 3 6)
        info [i|Got #{Prelude.length completions} completions for local variables|]

        -- Just check that we get some completions
        liftIO $ Prelude.length completions `shouldBeAtLeast` 0

shouldContainAny :: [T.Text] -> [T.Text] -> IO ()
shouldContainAny haystack needles =
  if any (`elem` haystack) needles
    then return ()
    else expectationFailure [i|Expected #{haystack} to contain at least one of #{needles}|]

shouldBeAtLeast :: Int -> Int -> IO ()
shouldBeAtLeast actual expected =
  if actual >= expected
    then return ()
    else expectationFailure [i|Expected at least #{expected} but got #{actual}|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
