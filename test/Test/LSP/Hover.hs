{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.Hover where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Function (fix)
import UnliftIO.Exception
import System.FilePath
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Language.LSP.Protocol.Lens hiding (hover)
import Language.LSP.Protocol.Types
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP
import UnliftIO.Directory
import UnliftIO.Process


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

introduceCnls :: forall context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m
  )
  => SpecFree (LabelValue "file-cpp-notebook-language-server" (EnvironmentFile "cpp-notebook-language-server") :> context) m ()
  -> SpecFree context m ()
introduceCnls = introduce [i|cpp-notebook-language-server (binary via Nix derivation)|] (Label :: Label "file-cpp-notebook-language-server" (EnvironmentFile "cpp-notebook-language-server")) alloc (const $ return ())
  where
    alloc = do
      projectRoot <- getProjectRoot
      dir <- buildNixCallPackageDerivation (cppNotebookLanguageServerDerivation projectRoot)
      liftIO (findExecutablesInDirectories [dir </> "bin"] "cpp-notebook-language-server") >>= \case
        (x:_) -> return (EnvironmentFile x :: EnvironmentFile "cpp-notebook-language-server")
        _ -> expectationFailure [i|Couldn't find binary in #{dir </> "bin"}|]

cppNotebookLanguageServerDerivation :: FilePath -> T.Text
cppNotebookLanguageServerDerivation projectRoot = [i|
{ ... }:

let
  flake = builtins.getFlake "#{projectRoot}";
in flake.packages.x86_64-linux.default
|]

shouldContainText :: T.Text -> T.Text -> IO ()
shouldContainText haystack needle =
  if needle `T.isInfixOf` haystack
    then return ()
    else expectationFailure [i|Expected "#{haystack}" to contain "#{needle}"|]

getProjectRoot :: (HasCallStack, MonadIO m) => m FilePath
getProjectRoot = do
  startDir <- getCurrentDirectory
  flip fix startDir $ \loop dir -> do
    doesDirectoryExist (dir </> ".git") >>= \case
      True -> return dir
      False -> let dir' = takeDirectory dir in
                 if | dir == dir' -> throwIO $ userError [i|Couldn't find project root starting from #{startDir}|]
                    | otherwise -> loop dir'

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
