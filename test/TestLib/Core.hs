{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}

module TestLib.Core where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List as L
import Data.Row.Records
import Data.Text as T
import GHC.Stack
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.QuickCheck as Q
import Test.Sandwich
import TestLib.Util
import UnliftIO.Exception


testChange :: forall a m. (
  Transformer a, Eq a, Show a, MonadIO m
  ) => Params a -> Doc -> TextDocumentContentChangeEvent -> m Property
testChange = testChange' @a (const [])

testChange' :: forall a m. (
  Transformer a, Eq a, Show a, MonadIO m
  ) => ([TextDocumentContentChangeEvent] -> [Property]) -> Params a -> Doc -> TextDocumentContentChangeEvent -> m Property
testChange' extraProps params docLines change = do
  -- Expected un-projected document after the change
  let docLines' = applyChanges [change] docLines

  (projectedBefore, transformer :: a) <- project params docLines
  (projectedAfter, reprojectedTransformer :: a) <- project params docLines'

  (changes, transformer') <- handleDiff params docLines change transformer
  let afterFromChange' = applyChanges changes projectedBefore

  return $ conjoin ([
    -- Applying the change' returned from handleDiff to the projected before value gives expected projected value
    afterFromChange' === projectedAfter

    -- The re-projected transformer matches the one we got back from handleDiff
    , reprojectedTransformer === transformer'
    ] <> extraProps changes)
