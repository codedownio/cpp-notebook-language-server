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
  , doRawSession

  , introduceMaybeBubblewrap
  , introduceCnls

  , transformRoundTrip
  , transformRoundTripCode
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List as L
import Data.Function (fix)
import Data.String.Interpolate
import Data.Text
import GHC.Stack
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types
import Language.LSP.Transformer hiding ((:>))
import System.FilePath
import System.IO
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.LSP.Test.Helpers as Helpers
import qualified System.Directory as Dir


doNotebookSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession = doSession "main.ipynb"

doRawSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doRawSession = doSession "main.cpp"

doSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => FilePath -> T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession fileName codeToUse cb = do
  cppNotebookLSPath <- askFile @"cpp-notebook-language-server"
  clangdPath <- askFile @"clangd"

  withSystemTempDirectory "cpp-lsp-test" $ \tmpDir -> do
    let testFile = tmpDir </> fileName

    -- Write the notebook code
    liftIO $ TIO.writeFile testFile codeToUse

    -- -- Create compile_commands.json for clangd
    -- let compileCommandsPath = tmpDir </> "compile_commands.json"
    -- liftIO $ writeFile compileCommandsPath $ unlines [
    --   "[{",
    --   "  \"directory\": \"" ++ tmpDir ++ "\",",
    --   "  \"command\": \"clang++ -std=c++17 test.cpp\",",
    --   "  \"file\": \"" ++ tmpDir </> "test.cpp" ++ "\"",
    --   "}]"
    --   ]

    let lspConfig = Helpers.LanguageServerConfig {
          lspConfigName = "cpp-notebook-language-server"
          , lspConfigVersion = Nothing
          , lspConfigDescription = Just "C++ notebook language server"
          , lspConfigDisplayName = Just "C++ Notebook Language Server"
          , lspConfigIcon = Nothing
          , lspConfigExtensions = [".cpp", ".hpp", ".cc", ".cxx", ".c++", ".h", ".hxx"]
          , lspConfigAttrs = S.fromList ["cpp"]
          , lspConfigType = Helpers.LanguageServerTypeStream
          , lspConfigPrimary = Just True
          , lspConfigArgs = [
              T.pack cppNotebookLSPath
              , "--wrapped-server", T.pack clangdPath
              , "--log-level", "debug"
              ]
          , lspConfigLanguageId = Just LanguageKind_CPP
          , lspConfigInitializationOptions = Nothing
          , lspConfigNotebookSuffix = ".cpp"
          , lspConfigKernelName = Nothing
          , lspConfigEnv = Nothing
          , lspConfigFile = Nothing
          , lspConfigIsBuiltIn = Just False
          }

    pathToUse <- getBasicPath
    closure <- getCppLspClosure cppNotebookLSPath clangdPath pathToUse

    let lspSessionOptions = (Helpers.defaultLspSessionOptions lspConfig) {
          Helpers.lspSessionOptionsInitialFileName = fileName
          , Helpers.lspSessionOptionsInitialLanguageKind = LanguageKind_CPP
          , Helpers.lspSessionOptionsInitialCode = codeToUse
          , Helpers.lspSessionOptionsExtraFiles = []
          , Helpers.lspSessionOptionsPathEnvVar = pathToUse
          , Helpers.lspSessionOptionsReadOnlyBinds = closure
          , Helpers.lspSessionOptionsModifySessionConfig = \x -> x { LSP.messageTimeout = 10 }
          }

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
  info [i|Got Nix closure with #{L.length closure} entries|]
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
in flake.packages.x86_64-linux.staticWrapped
|]


getProjectRoot :: (HasCallStack, MonadIO m) => m FilePath
getProjectRoot = do
  startDir <- getCurrentDirectory
  flip fix startDir $ \loop dir -> do
    doesDirectoryExist (dir </> ".git") >>= \case
      True -> return dir
      False -> let dir' = takeDirectory dir in
                 if | dir == dir' -> throwIO $ userError [i|Couldn't find project root starting from #{startDir}|]
                    | otherwise -> loop dir'


transformRoundTripCode :: (MonadIO m, MonadFail m) => Text -> Position -> Position -> m ()
transformRoundTripCode testCode from to = do
  let inputDoc = listToDoc (T.splitOn "\n" testCode)
  (_, sifter :: DeclarationSifter, _) <- project (DeclarationSifterParams "cling-parser" "__notebook_exec") inputDoc
  let params = DeclarationSifterParams "cling-parser" "__notebook_exec"
  transformRoundTrip params sifter from to

transformRoundTrip :: (MonadIO m, MonadFail m) => Params DeclarationSifter -> DeclarationSifter -> Position -> Position -> m ()
transformRoundTrip params sifter from to = do
  Just pos <- return $ transformPosition params sifter from
  pos `shouldBe` to

  Just pos' <- return $ untransformPosition params sifter to
  pos' `shouldBe` from
