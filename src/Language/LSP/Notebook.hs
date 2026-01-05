{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  CppNotebookTransformer
  , CppNotebookTransformerV2
  , transformerParams
  , transformerParamsV2
  , idTransformerParams
  , idTransformerParamsV2
  ) where

import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Notebook.StripDirective
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Notebook.ExecutableWrapper
import Language.LSP.Transformer


-- Original transformer (for compatibility)
type CppNotebookTransformer =
  StripDirective
  :> HeadTailTransformer -- Wrap the whole doc in a function

transformerParams :: Params CppNotebookTransformer
transformerParams =
  SDParams True
  :> (["int main() {"], ["}"])

idTransformerParams :: Params CppNotebookTransformer
idTransformerParams =
  SDParams False
  :> ([], [])

-- New C++ specific transformer chain
type CppNotebookTransformerV2 =
  DeclarationSifter              -- Move includes, using, classes, functions to top
  :> ExecutableWrapper           -- Wrap everything else in single function
  :> HeadTailTransformer         -- Add main function wrapper

transformerParamsV2 :: Params CppNotebookTransformerV2  
transformerParamsV2 =
  DeclarationSifterParams "minimal-parser"
  :> ExecutableWrapperParams "__notebook_exec" "minimal-parser"
  :> (["int main() {", "  __notebook_exec();", "  return 0;"], ["}"])

idTransformerParamsV2 :: Params CppNotebookTransformerV2
idTransformerParamsV2 =
  DeclarationSifterParams "true"  -- dummy command that does nothing
  :> ExecutableWrapperParams "__notebook_exec" "true" 
  :> ([], [])
