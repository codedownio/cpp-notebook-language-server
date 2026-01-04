{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  CppNotebookTransformer
  , transformerParams
  , idTransformerParams
  ) where

import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer


type CppNotebookTransformer =
  StripDirective
  :> HeadTailTransformer -- Wrap the whole doc in a function

transformerParams :: Params CppNotebookTransformer
transformerParams =
  SDParams True
  :> (["fn main() {"], ["}"])

idTransformerParams :: Params CppNotebookTransformer
idTransformerParams =
  SDParams False
  :> ([], [])
