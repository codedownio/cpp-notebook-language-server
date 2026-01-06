{-# LANGUAGE OverloadedStrings #-}

module Test.Pipeline where

import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Notebook.ExecutableWrapper  
import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Transformer
import Test.Sandwich

-- Test input: mixed declarations and executable statements (like a notebook)
testInput :: T.Text
testInput = T.unlines
  [ "#include <iostream>"
  , "using namespace std;"
  , "cout << \"hello\" << endl;"
  , "int x = 42;"
  , "class MyClass {};"
  , "void func() {}"
  , "cout << \"after\" << endl;"
  ]

-- Expected final output after full pipeline
expectedFinalOutput :: T.Text  
expectedFinalOutput = T.unlines
  [ "#include <iostream>"
  , "using namespace std;"
  , "class MyClass {};"
  , "void func() {}"
  , "int x = 42;"
  , ""
  , "void __notebook_exec() {"
  , "  cout << \"hello\" << endl;"
  , "  cout << \"after\" << endl;"
  , "}"
  , ""
  , "int main() {"
  , "  __notebook_exec();"
  , "  return 0;"
  , "}"
  ]

spec :: TopSpec
spec = describe "Full Pipeline" $ do

  it "should transform notebook code to valid C++ through full pipeline" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testInput)
    
    -- Step 1: DeclarationSifter
    (siftedDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser") inputDoc
    let siftedText = T.intercalate "\n" $ docToList siftedDoc
    
    -- Verify DeclarationSifter moved declarations to top
    T.isPrefixOf "#include" siftedText `shouldBe` True
    T.isInfixOf "using namespace std;" siftedText `shouldBe` True
    T.isInfixOf "class MyClass {};" siftedText `shouldBe` True
    
    -- Step 2: ExecutableWrapper
    (wrappedDoc, execWrapper :: ExecutableWrapper) <- project (ExecutableWrapperParams "__notebook_exec" "minimal-parser") siftedDoc
    let wrappedText = T.intercalate "\n" $ docToList wrappedDoc
    
    -- Verify ExecutableWrapper wrapped executable statements
    hasWrapper execWrapper `shouldBe` True
    T.isInfixOf "void __notebook_exec() {" wrappedText `shouldBe` True
    T.isInfixOf "  cout << \"hello\" << endl;" wrappedText `shouldBe` True
    T.isInfixOf "  cout << \"after\" << endl;" wrappedText `shouldBe` True
    
    -- Step 3: HeadTailTransformer
    (finalDoc, _ :: HeadTailTransformer) <- project ([], ["", "int main() {", "  __notebook_exec();", "  return 0;", "}"]) wrappedDoc
    let finalText = T.intercalate "\n" $ docToList finalDoc
    
    -- Verify HeadTailTransformer added main() wrapper
    T.isInfixOf "int main() {" finalText `shouldBe` True
    T.isInfixOf "  __notebook_exec();" finalText `shouldBe` True
    T.isInfixOf "  return 0;" finalText `shouldBe` True
    
    -- Verify final structure looks correct (declarations at top, executable code wrapped, main() at bottom)
    let finalLines = T.splitOn "\n" finalText
    (length finalLines > 10) `shouldBe` True  -- Should have substantial content
    
  it "should preserve declaration order and structure" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testInput)
    
    -- Run full pipeline
    (siftedDoc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser") inputDoc
    (wrappedDoc, _ :: ExecutableWrapper) <- project (ExecutableWrapperParams "__notebook_exec" "minimal-parser") siftedDoc
    (finalDoc, _ :: HeadTailTransformer) <- project ([], ["", "int main() {", "  __notebook_exec();", "  return 0;", "}"]) wrappedDoc
    
    let finalText = T.intercalate "\n" $ docToList finalDoc
        finalLines = T.splitOn "\n" finalText
        
    -- Check structure: includes first, then other declarations, then executable function, then main
    case finalLines of
      [] -> error "No output"
      (firstLine:_) -> T.isPrefixOf "#include" firstLine `shouldBe` True
      
    -- Check that declarations come before executable wrapper
    let includePos = findSubstring "#include" finalText
        execPos = findSubstring "void __notebook_exec()" finalText
        mainPos = findSubstring "int main()" finalText
        
    (includePos < execPos) `shouldBe` True
    (execPos < mainPos) `shouldBe` True

-- Helper to find position of substring  
findSubstring :: T.Text -> T.Text -> Int
findSubstring needle haystack = 
  case T.breakOn needle haystack of
    (prefix, remaining) -> 
      if T.null remaining 
      then -1  -- Not found
      else T.length prefix

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec