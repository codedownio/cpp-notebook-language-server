{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Transform.ClientNot.DidSave where

import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Transform.Util


doDidSave :: TransformerMonad m => DocumentState -> SendExtraNotificationFn m -> m ()
doDidSave (DocumentState {..}) sendExtraNotification = do
  whenServerCapabilitiesSatisfy supportsWillSave $ \_ ->
    sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentWillSave $ WillSaveTextDocumentParams {
      _textDocument = TextDocumentIdentifier newUri
      , _reason = TextDocumentSaveReason_AfterDelay
      }

  liftIO $ T.writeFile newPath (Rope.toText curLines')

  whenServerCapabilitiesSatisfy supportsSave $ \maybeSaveOptions ->
    sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentDidSave $ DidSaveTextDocumentParams {
      _textDocument = TextDocumentIdentifier newUri
      , _text = case maybeSaveOptions of
          Just (SaveOptions {_includeText=(Just True)}) -> Just (Rope.toText curLines')
          _ -> Nothing
      }
