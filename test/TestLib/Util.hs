{-# LANGUAGE OverloadedLabels #-}

module TestLib.Util where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List as L
import Data.Text as T
import GHC.Stack
import qualified Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Test.QuickCheck as Q
import Test.Sandwich
import UnliftIO.Exception


p :: Int -> Int -> Position
p l c = Position (fromIntegral l) (fromIntegral c)

isSingleLineChange :: [TextDocumentContentChangeEvent] -> [Property]
isSingleLineChange [TextDocumentContentChangeEvent (InL partialChange)] =
  [l1 === l2 .&&. (L.length (T.splitOn "\n" (partialChange ^. Lens.text)) === 1)]
  where
    Range (Position l1 _c1) (Position l2 _c2) = partialChange ^. Lens.range
isSingleLineChange [TextDocumentContentChangeEvent (InR _wholeDocumentChange)] =
  [True === False]
isSingleLineChange [] = []
isSingleLineChange _ = error "Unexpected TextDocumentContentChangeEvent"

mkChange :: (UInt, UInt) -> (UInt, UInt) -> Maybe UInt -> Text -> TextDocumentContentChangeEvent
mkChange (l1, c1) (l2, c2) maybeRangeLen t = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial {
  _range = Range (Position l1 c1) (Position l2 c2)
  , _rangeLength = maybeRangeLen
  , _text = t
  }

quickCheckSingleProp :: (MonadIO m, Testable prop, MonadLogger m) => prop -> m ()
quickCheckSingleProp prop = do
  liftIO (quickCheckWithResult (stdArgs { Q.chatty = False, Q.maxSuccess = 1 }) prop) >>= \case
    Q.Success {..} -> info (T.pack output)
    x -> throwIO $ Reason (Just callStack) (output x)
