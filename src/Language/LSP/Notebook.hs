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
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Notebook.ExecutableWrapper
import Language.LSP.Transformer


-- Original transformer (for compatibility) - simplified without StripDirective
type CppNotebookTransformer = HeadTailTransformer

transformerParams :: Params CppNotebookTransformer
transformerParams = (["int main() {"], ["}"])

idTransformerParams :: Params CppNotebookTransformer
idTransformerParams = ([], [])

-- New C++ specific transformer chain
type CppNotebookTransformerV2 =
  DeclarationSifter              -- Move includes, using, classes, functions to top
  :> ExecutableWrapper           -- Wrap everything else in single function
  :> HeadTailTransformer         -- Add main function wrapper

transformerParamsV2 :: Params CppNotebookTransformerV2  
transformerParamsV2 =
  DeclarationSifterParams "minimal-parser" True "__notebook_exec"  -- Now handles both sifting and wrapping
  :> ExecutableWrapperParams "__notebook_exec" "minimal-parser"
  :> (["int main() {", "  __notebook_exec();", "  return 0;"], ["}"])

idTransformerParamsV2 :: Params CppNotebookTransformerV2
idTransformerParamsV2 =
  DeclarationSifterParams "true" False ""  -- dummy command that does nothing, no wrapping
  :> ExecutableWrapperParams "__notebook_exec" "true" 
  :> ([], [])
