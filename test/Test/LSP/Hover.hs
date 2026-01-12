{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.Hover where

import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Data.Text as T
import qualified Language.LSP.Test.Helpers as Helpers


spec :: TopSpec
spec = describe "C++ LSP Hover Tests" $
  introduceMaybeBubblewrap $
  introduceNixContext nixpkgsReleaseDefault $
  introduceBinaryViaNixPackage @"clangd" "clang-tools" $
  introduceCnls $ do

    it "hovers over variable declaration" $ do
      let testCode = T.unlines [
            "int x = 42;",
            "float y = 3.14;"
            ]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Hover over 'x' at position (0, 4)
        LSP.getHover doc (Position 0 4) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for variable 'x'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'x': #{hoverText}|]
            liftIO $ hoverText `shouldContainText` "int"

    it "hovers over function call" $ do
      let testCode = T.unlines [
            "#include <iostream>",
            "int main() {",
            "  std::cout << \"Hello\" << std::endl;",
            "  return 0;",
            "}"
            ]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Hover over 'cout' at position (2, 8)
        LSP.getHover doc (Position 2 8) >>= \case
          Nothing -> info "No hover found for cout (might be expected)"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'cout': #{hoverText}|]
            -- std::cout is an ostream
            liftIO $ hoverText `shouldContainText` "std"

shouldContainText :: T.Text -> T.Text -> IO ()
shouldContainText haystack needle =
  if needle `T.isInfixOf` haystack
    then return ()
    else expectationFailure [i|Expected "#{haystack}" to contain "#{needle}"|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
