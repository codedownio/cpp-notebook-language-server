{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.LSP (
  doNotebookSession
  , getBasicPath
  , getCppLspClosure
  , introduceMaybeBubblewrap
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Text.IO as TIO
import Language.LSP.Protocol.Types
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import qualified System.Directory as Dir
import System.FilePath
import System.IO
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import UnliftIO.Directory
import UnliftIO.Exception (bracket)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Process


doNotebookSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession codeToUse cb = do
  cppNotebookLSPath <- askFile @"cpp-notebook-language-server"
  clangdPath <- askFile @"clangd"

  -- Get the basic PATH by running nix
  pathToUse <- getBasicPath

  withSystemTempDirectory "cpp-lsp-test" $ \tmpDir -> do
    let fileName = "main.ipynb"
    let testFile = tmpDir </> T.unpack fileName

    -- Write the notebook code
    liftIO $ TIO.writeFile testFile codeToUse

    -- Create compile_commands.json for clangd
    let compileCommandsPath = tmpDir </> "compile_commands.json"
    liftIO $ writeFile compileCommandsPath $ unlines [
      "[{",
      "  \"directory\": \"" ++ tmpDir ++ "\",",
      "  \"command\": \"clang++ -std=c++17 test.cpp\",",
      "  \"file\": \"" ++ tmpDir </> "test.cpp" ++ "\"",
      "}]"
      ]

    -- Configure the language server
    let lspConfig = Helpers.LanguageServerConfig {
          lspConfigName = "cpp-notebook-language-server"
          , lspConfigVersion = Nothing
          , lspConfigDescription = Just "C++ notebook language server"
          , lspConfigDisplayName = Just "C++ Notebook LSP"
          , lspConfigIcon = Nothing
          , lspConfigExtensions = [".cpp", ".hpp", ".cc", ".cxx", ".c++", ".h", ".hxx"]
          , lspConfigAttrs = S.fromList ["cpp"]
          , lspConfigType = Helpers.LanguageServerTypeStream
          , lspConfigPrimary = Just True
          , lspConfigArgs = [
              T.pack cppNotebookLSPath
              , "--wrapped-server", T.pack clangdPath
              ]
          , lspConfigLanguageId = Just LanguageKind_CPP
          , lspConfigInitializationOptions = Nothing
          , lspConfigNotebookSuffix = ".cpp"
          , lspConfigKernelName = Nothing
          , lspConfigEnv = Nothing
          , lspConfigFile = Nothing
          , lspConfigIsBuiltIn = Just False
          }

    -- Get the Nix closure for both binaries
    closure <- getCppLspClosure cppNotebookLSPath clangdPath pathToUse

    let lspSessionOptions = (Helpers.defaultLspSessionOptions lspConfig) {
          Helpers.lspSessionOptionsInitialFileName = T.unpack fileName
          , Helpers.lspSessionOptionsInitialLanguageKind = LanguageKind_CPP
          , Helpers.lspSessionOptionsInitialCode = codeToUse
          , Helpers.lspSessionOptionsExtraFiles = []
          , Helpers.lspSessionOptionsPathEnvVar = pathToUse
          , Helpers.lspSessionOptionsReadOnlyBinds = closure
          }

    -- Run the LSP session
    Helpers.withLspSession lspSessionOptions cb

getBasicPath :: (MonadUnliftIO m, MonadLogger m) => m FilePath
getBasicPath = do
  bracket (liftIO $ openFile "/dev/null" WriteMode) (liftIO . hClose) $ \devNullHandle -> do
    result <- readCreateProcess ((proc "nix" ["run", ".#print-basic-path"]) { std_err = UseHandle devNullHandle }) ""
    let ret = T.unpack $ T.strip $ T.pack result
    info [i|getBasicPath: #{ret}|]
    return ret

-- | Get the full Nix closure of cpp-notebook-language-server + clangd + PATH entries
getCppLspClosure :: (MonadUnliftIO m, MonadLogger m) => FilePath -> FilePath -> FilePath -> m [FilePath]
getCppLspClosure cppNotebookLSPath clangdPath pathToUse = do
  let paths = [cppNotebookLSPath, clangdPath] ++ splitSearchPath pathToUse
  closure <- (fmap T.unpack . Prelude.filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcess (
    proc "nix" (["path-info", "-r"] ++ paths)
    ) ""
  info [i|Got Nix closure with #{length closure} entries|]
  return closure

-- | Introduce maybeBubblewrap context, trying to find bwrap executable
-- Falls back to Nothing on macOS or if bwrap is not found
introduceMaybeBubblewrap :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m
  ) => SpecFree (LabelValue "maybeBubblewrap" (Maybe FilePath) :> context) m () -> SpecFree context m ()
introduceMaybeBubblewrap = introduceWith [i|maybeBubblewrap|] Helpers.maybeBubblewrap $ \action -> do
#ifdef darwin_HOST_OS
  void $ action Nothing
#else
  liftIO (Dir.findExecutable "bwrap") >>= \case
    Nothing -> do
      info "bubblewrap (bwrap) not found, proceeding without sandbox"
      void $ action Nothing
    Just path -> do
      info [i|Found bubblewrap at: #{path}|]
      void $ action (Just path)
#endif
