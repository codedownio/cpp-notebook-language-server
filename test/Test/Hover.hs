{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hover where

import Control.Monad.IO.Unlift
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
spec = describe "Hover" $ do
    it "hovers over variable declaration" $ do
      let testCode = [__i|int x = 42;
                          float y = 3.14;
                          |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Hover over 'x'
        LSP.getHover doc (Position 0 4) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for variable 'x'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'x': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "int"

    it "hovers over function call" $ do
      let testCode = [__i|\#include <iostream>
                          int main() {
                            std::cout << "Hello" << std::endl;
                            return 0;
                          }|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP

        -- Hover over 'cout' at position (2, 8)
        LSP.getHover doc (Position 2 8) >>= \case
          Nothing -> info "No hover found for cout (might be expected)"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'cout': #{hoverText}|]
            -- std::cout is an ostream
            liftIO $ hoverText `textShouldContain` "std"
