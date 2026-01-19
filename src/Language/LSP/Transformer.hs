{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}

module Language.LSP.Transformer (
  Doc

  , Transformer(..)
  , (:>)(..)
  , defaultHandleDiff

  , listToDoc
  , docToList

  , applyChange
  , applyChanges
  ) where

import Control.Lens ((^.))
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Diff.Myers
import qualified Data.Diff.Types as DT
import Data.Either
import Data.Kind
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types as J


type Doc = Rope.Rope

class Transformer a where
  type Params a

  getParams :: a -> Params a

  project :: MonadIO m => Params a -> Doc -> m (Doc, a, Either Text ())

  handleDiffMulti :: MonadIO m => Params a -> Doc -> [TextDocumentContentChangeEvent] -> a -> m ([TextDocumentContentChangeEvent], a)
  handleDiffMulti params before changes tx = do
    result <- foldM f ([], before, tx) changes
    let (finalChanges, _finalLines, finalTx) = result
    return (finalChanges, finalTx)
    where
      f :: MonadIO m => ([TextDocumentContentChangeEvent], Doc, a) -> TextDocumentContentChangeEvent -> m ([TextDocumentContentChangeEvent], Doc, a)
      f (changesSoFar, curLines, txSoFar) change = do
        (newChanges, tx') <- handleDiff params curLines change txSoFar
        return (changesSoFar <> newChanges, applyChanges [change] curLines, tx')

  handleDiff :: MonadIO m => Params a -> Doc -> TextDocumentContentChangeEvent -> a -> m ([TextDocumentContentChangeEvent], a)
  handleDiff = defaultHandleDiff

  transformPosition :: Params a -> a -> Position -> Maybe Position

  untransformPosition :: Params a -> a -> Position -> Maybe Position

data (a :: Type) :> (b :: Type) = a :> b
  deriving Show
infixr :>

instance (Transformer a, Transformer b) => Transformer (a :> b) where
  type Params (a :> b) = Params a :> Params b
  getParams (x :> y) = getParams x :> getParams y
  project (xParams :> yParams) lines = do
    (lines', x, eitherErr1) <- project xParams lines
    (lines'', y, eitherErr2) <- project yParams lines'
    let overallErr = case lefts [eitherErr1, eitherErr2] of
          [] -> Right ()
          xs -> Left (T.intercalate "; " xs)
    return (lines'', x :> y, overallErr)
  handleDiff (xParams :> yParams) before change (x :> y) = do
    (change', x') <- handleDiff xParams before change x
    (beforeTransformed, _, _eitherErr1) <- project @a xParams before
    (change'', y') <- handleDiffMulti yParams beforeTransformed change' y
    return (change'', x' :> y')
  transformPosition (xParams :> yParams) (x :> y) p = transformPosition xParams x p >>= transformPosition yParams y
  untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition yParams y p >>= untransformPosition xParams x

-- Default implementation uses diff.
defaultHandleDiff :: forall a m. (Transformer a, MonadIO m) => Params a -> Doc -> TextDocumentContentChangeEvent -> a -> m ([TextDocumentContentChangeEvent], a)
defaultHandleDiff params before change _transformer = do
  (before', _ :: a, _eitherErrBefore) <- project params before
  let after = applyChanges [change] before
  (after', transformer' :: a, _eitherErrAfter) <- project params after
  let change' = fmap repackChangeEvent $ diffTextsToChangeEventsConsolidate (Rope.toText before') (Rope.toText after')
  return (change', transformer')
  where
    repackChangeEvent (DT.ChangeEvent range text) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial (repackRange range) Nothing text
    repackRange (DT.Range (DT.Position l1 c1) (DT.Position l2 c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

-- * Applying changes

listToDoc :: [Text] -> Doc
listToDoc = Rope.fromText . T.intercalate "\n"

docToList :: Doc -> [Text]
docToList = T.splitOn "\n" . Rope.toText

-- * Based on code from haskell-lsp/lsp (https://github.com/haskell/lsp/tree/master/lsp)
-- Under MIT license

applyChanges :: [J.TextDocumentContentChangeEvent] -> Rope -> Rope
applyChanges changes rope = L.foldl' (flip applyChange) rope changes

applyChange :: J.TextDocumentContentChangeEvent -> Rope -> Rope
applyChange (J.TextDocumentContentChangeEvent (InL partial)) str =
  changeChars str (Rope.Position (fromIntegral sl) (fromIntegral sc)) (Rope.Position (fromIntegral fl) (fromIntegral fc)) (partial ^. text)
  where
    J.Range (J.Position sl sc) (J.Position fl fc) = partial ^. range
applyChange (J.TextDocumentContentChangeEvent (InR whole)) _ = Rope.fromText (whole ^. text)

changeChars :: Rope -> Rope.Position -> Rope.Position -> Text -> Rope
changeChars str start finish new = mconcat [before', Rope.fromText new, after]
 where
   (before, after) = Rope.splitAtPosition finish str
   (before', _) = Rope.splitAtPosition start before
