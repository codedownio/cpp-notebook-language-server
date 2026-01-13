{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


-- * Transform

transformRange :: CppNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => CppNotebookTransformer -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition (getParams tx) tx)
  >>= traverseOf (range . end) (transformPosition (getParams tx) tx)

-- * Untransform

untransformRange :: CppNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => CppNotebookTransformer -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition (getParams tx) tx)
  >>= traverseOf (range . end) (untransformPosition (getParams tx) tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => CppNotebookTransformer -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition (getParams tx) tx)
  >>= traverseOf (range . _Just . end) (untransformPosition (getParams tx) tx)

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
