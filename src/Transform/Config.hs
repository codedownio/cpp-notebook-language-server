{-# LANGUAGE RankNTypes #-}

module Transform.Config (cppNotebookConfig) where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Language.LSP.Transformer (Doc)
import Text.Regex.PCRE.Light (Regex)
import UnliftIO.Exception


cppNotebookConfig :: Int -> NotebookConfig CppNotebookTransformer
cppNotebookConfig didSaveDebounceMs = NotebookConfig {
  notebookConfigHandles = const True
  , notebookConfigDidSaveDebounceMs = Just didSaveDebounceMs
  , notebookConfigTransformerParamsFor = \u -> if isNotebook u then transformerParams else idTransformerParams
  , notebookConfigMakeShadowDocument = makeShadowDocument
  , notebookConfigUnopenedUri = addExtensionToUri ".cpp"
  , notebookConfigOnDocumentsChanged = pure ()
  , notebookConfigTransformInitialize = pure
  , notebookConfigCustomUriMethods = ["clangd/analyzerStatus"]
  , notebookConfigIgnoreSymbol = const False
  , notebookConfigLanguageName = "cpp"
  }

makeShadowDocument :: TransformerMonad CppNotebookTransformer n => Uri -> Doc -> n (Uri, FilePath, Regex)
makeShadowDocument origUri _projected = do
  newUri <- if isNotebook origUri then addExtensionToUri ".cpp" origUri else pure origUri

  newPath <- case uriToFilePath newUri of
    Nothing -> throwIO $ userError [i|Failed to convert new URI to file path: #{newUri}|]
    Just x -> pure x

  pure (newUri, newPath, mkDocRegex (T.pack newPath))
