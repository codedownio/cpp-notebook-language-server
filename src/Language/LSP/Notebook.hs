{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  CppNotebookTransformer
  , transformerParams
  , idTransformerParams
  ) where

import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Transformer


-- C++ notebook transformer - DeclarationSifter with executable wrapping
type CppNotebookTransformer = DeclarationSifter

transformerParams :: Params CppNotebookTransformer
transformerParams = DeclarationSifterParams "cling-parser" "__notebook_exec"

idTransformerParams :: Params CppNotebookTransformer
idTransformerParams = DeclarationSifterParams "true" ""
