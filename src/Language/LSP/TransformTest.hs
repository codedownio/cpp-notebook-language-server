module Language.LSP.TransformTest where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Notebook.ExecutableWrapper
import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Transformer

-- Sample C++ notebook code with mixed declarations and executable statements
sampleCppCode :: T.Text
sampleCppCode = [i|
\#include <iostream>
\#include <vector>

using namespace std;

int x = 42;

void myFunction(int param) {
    cout << "Function called with: " << param << endl;
}

class MyClass {
public:
    int member;
    void method() {
        cout << "Method called" << endl;
    }
};

double y = 3.14;

cout << "Hello world!" << endl;
cout << "x = " << x << ", y = " << y << endl;

vector<int> nums = {1, 2, 3, 4, 5};

for (int i : nums) {
    cout << i << " ";
}
cout << endl;
|]

main :: IO ()
main = do
  putStrLn "=== C++ Notebook Transformer Demo ==="
  putStrLn "\n--- Original Code ---"
  putStrLn $ T.unpack sampleCppCode
  
  putStrLn "\n--- Applying V2 Transformer Chain ---"
  putStrLn "Chain: ImportSifter :> ExecutableWrapper :> HeadTailTransformer"
  
  let inputDoc = listToDoc (T.splitOn "\n" sampleCppCode)
  (transformedDoc, transformer :: CppNotebookTransformerV2) <- project transformerParamsV2 inputDoc
  
  putStrLn "\n--- Transformed Code ---"
  putStrLn $ T.unpack $ T.intercalate "\n" $ docToList transformedDoc
  
  putStrLn "\n--- Transformer State ---"
  putStrLn $ "Generated transformer: " ++ show transformer
  
  putStrLn "\n=== Testing Individual Components ==="
  
  putStrLn "\n--- Step 1: DeclarationSifter Only ---"
  (step1Doc, _ :: DeclarationSifter) <- project (DeclarationSifterParams "minimal-parser" True "__notebook_exec") inputDoc
  putStrLn "After moving declarations to top and wrapping executables:"
  putStrLn $ T.unpack $ T.intercalate "\n" $ docToList step1Doc
  
  putStrLn "\n--- Step 2: ExecutableWrapper Only ---" 
  (step2Doc, _ :: ExecutableWrapper) <- project (ExecutableWrapperParams "__exec" "minimal-parser") inputDoc
  putStrLn "After wrapping executable statements:"
  putStrLn $ T.unpack $ T.intercalate "\n" $ docToList step2Doc
  
  putStrLn "\n--- Step 3: HeadTailTransformer Only ---"
  (step3Doc, _ :: HeadTailTransformer) <- project (["int main() {"], ["}"]) inputDoc  
  putStrLn "After adding main function wrapper:"
  putStrLn $ T.unpack $ T.intercalate "\n" $ docToList step3Doc
  
  putStrLn "\n=== Transformation Complete ==="