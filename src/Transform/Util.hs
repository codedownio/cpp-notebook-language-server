{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Transform.Util where

import Control.Lens hiding ((:>), (<.>))
import Control.Lens.Regex.Text
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Transformer
import Network.URI
import System.FilePath
import qualified System.Random as R
import UnliftIO.MVar


type SendExtraNotificationFn n = forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ()

-- * whenAnything

whenAnything :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> (Uri -> n a) -> n a
whenAnything params = whenAnything' (params ^. (textDocument . uri)) params

whenAnythingByInitialParams :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> c -> (Uri -> n c) -> n c
whenAnythingByInitialParams params = whenAnything' (params ^. (textDocument . uri))

whenAnything' :: (MonadLoggerIO n) => Uri -> a -> (Uri -> n a) -> n a
whenAnything' uri _params action = action uri

-- * whenNotebook

whenNotebook :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> (Uri -> n a) -> n a
whenNotebook params = whenNotebook' (params ^. (textDocument . uri)) params

whenNotebookUri :: (MonadLoggerIO n, HasUri a Uri) => a -> (Uri -> n a) -> n a
whenNotebookUri params = whenNotebook' (params ^. uri) params

whenNotebookByInitialParams :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> c -> (Uri -> n c) -> n c
whenNotebookByInitialParams params = whenNotebook' (params ^. (textDocument . uri))

whenNotebook' :: (MonadLoggerIO n) => Uri -> a -> (Uri -> n a) -> n a
whenNotebook' uri params notebookParams = if
  | isNotebook uri -> notebookParams uri
  | otherwise -> pure params

isNotebook :: Uri -> Bool
isNotebook uri = case parseURIReference (T.unpack (getUri uri)) of
  Nothing -> False
  Just (URI {..}) -> ".ipynb" `L.isSuffixOf` fmap C.toLower uriPath

-- * whenNotebookResult

whenReverseLookupUri :: (TransformerMonad n, MonadLoggerIO n, HasUri a Uri) => a -> (DocumentState -> n a) -> n a
whenReverseLookupUri params = whenReverseLookupUri' (params ^. uri) params

-- | Note that this takes in server URIs and calls the callback with original URIs
-- TODO: make this more efficient
whenReverseLookupUri' :: (TransformerMonad n) => Uri -> a -> (DocumentState -> n a) -> n a
whenReverseLookupUri' uri params cb = do
  txDocumentsVar <- asks transformerDocuments
  txDocuments <- readMVar txDocumentsVar
  flip fix (M.toList txDocuments) $ \loop items -> case items of
    [] -> return params
    ((_, ds@(DocumentState {..})):rest) ->
      if | newUri == uri -> cb ds
         | otherwise -> loop rest

-- * TransformerMonad

type TransformerMonad n = (
  MonadLoggerIO n
  , MonadReader TransformerState n
  , MonadUnliftIO n
  , MonadFail n
  )

-- * TransformerState

data DocumentState = DocumentState {
  transformer :: CppNotebookTransformer
  , curLines :: Doc
  , curLines' :: Doc
  , origUri :: Uri
  , newUri :: Uri
  , newPath :: FilePath
  , referenceRegex :: Regex
  , documentUuid :: UUID
  , debouncedDidChange :: IO ()
  }

data TransformerState = TransformerState {
  transformerDocuments :: MVar (M.Map Text DocumentState)
  , transformerInitializeParams :: MVar (Maybe InitializeParams)
  , transformerInitializeResult :: MVar (Maybe InitializeResult)
  , transformerShadowDir :: FilePath
  , transformerDidSaveDebouncePeriodMs :: Int
  }

-- * Transformers

newTransformerState :: (MonadIO m) => FilePath -> Int -> m TransformerState
newTransformerState shadowDir didSaveDebouncePeriodMs = TransformerState
  <$> newMVar mempty
  <*> newMVar Nothing
  <*> newMVar Nothing
  <*> pure shadowDir
  <*> pure didSaveDebouncePeriodMs

lookupTransformer :: TransformerMonad m => Uri -> m (Maybe DocumentState)
lookupTransformer uri = do
  TransformerState {..} <- ask
  M.lookup (getUri uri) <$> readMVar transformerDocuments

withTransformer :: (TransformerMonad n) => a -> (DocumentState -> n a) -> Uri -> n a
withTransformer def cb uri = do
  lookupTransformer uri >>= \case
    Nothing -> do
      logWarnN [i|Couldn't find expected transformer for uri #{uri}|]
      return def
    Just tx -> cb tx

modifyTransformer :: (TransformerMonad n) => a -> (DocumentState -> n (DocumentState, a)) -> Uri -> n a
modifyTransformer def cb uri = do
  TransformerState {..} <- ask
  modifyMVar transformerDocuments $ \m -> case M.lookup (getUri uri) m of
    Nothing -> return (m, def)
    Just tx -> do
      (tx', ret) <- cb tx
      return (M.insert (getUri uri) tx' m, ret)

-- * Checking server capabilities

whenServerCapabilitiesSatisfy :: TransformerMonad n => (ServerCapabilities -> (Bool, a)) -> (a -> n ()) -> n ()
whenServerCapabilitiesSatisfy cb action = do
  initializeResultVar <- asks transformerInitializeResult
  readMVar initializeResultVar >>= \case
    Just ((cb . (^. capabilities)) -> (True, sideValue)) -> action sideValue
    _ -> return ()

supportsWillSave :: ServerCapabilities -> (Bool, ())
supportsWillSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_willSave=(Just True)}))) }) = (True, ())
supportsWillSave _ = (False, ())

supportsSave :: ServerCapabilities -> (Bool, Maybe SaveOptions)
supportsSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_save}))) }) = case _save of
  Nothing -> (False, Nothing)
  Just (InL x) -> (x, Nothing)
  Just (InR saveOptions) -> (True, Just saveOptions)
supportsSave _ = (False, Nothing)

-- * Random identifiers

-- Note: for a UUID to appear in a Kubernetes name, it needs to match this regex
-- [a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*'
uuidLetters :: [Char]
uuidLetters = ['a'..'z'] ++ ['A'..'Z']

numUUIDLetters :: Int
numUUIDLetters = L.length uuidLetters

makeUUID' :: MonadIO m => Int -> m String
makeUUID' n = replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1))

-- * Misc

addExtensionToUri :: (MonadLogger m) => String -> Uri -> m Uri
addExtensionToUri ext u@(Uri t) = case parseURIReference (T.unpack t) of
  Nothing -> do
    logErrorN [i|Couldn't parse URI: #{t}|]
    return u
  Just uri -> return $ Uri $ T.pack $ show $ uri { uriPath = uriPath uri <.> ext }

flipTuple :: (b, a) -> (a, b)
flipTuple (x, y) = (y, x)
