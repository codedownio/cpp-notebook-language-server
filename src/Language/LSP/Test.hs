
module Language.LSP.Test where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.CppParser
import System.Exit (ExitCode(..))
import UnliftIO.Process


testCode :: T.Text
testCode = [i|

\#include <iostream>

using namespace std;

int x = 42;

void myFunction(int param) {
    return;
}

class MyClass {
public:
    int member;
    void method();
 };

double y = 42.0;
cout << "hello: " << y << endl;

|]

main :: IO ()
main = do
  putStrLn "=== Testing cling-parser integration ==="
  putStrLn "\n--- Input C++ code ---"
  putStrLn $ T.unpack testCode

  putStrLn "\n--- Calling cling-parser ---"
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc "cling-parser" []) (T.unpack testCode)

  case exitCode of
    ExitSuccess -> do
      let jsonOutput = T.pack stdout
      putStrLn "cling-parser output:"
      putStrLn stdout

      putStrLn "\n--- Parsing with CppParser ---"
      case parseCppDeclarations jsonOutput of
        Left err -> do
          putStrLn $ "Failed to parse JSON: " ++ err
        Right decls -> do
          putStrLn $ "Successfully parsed " ++ show (length decls) ++ " declarations:"
          mapM_ printDeclaration decls

          putStrLn "\n--- Analysis ---"
          let imports = filter isImportLike decls
          let executables = filter isExecutableStatement decls
          putStrLn $ "Import-like declarations: " ++ show (length imports)
          mapM_ (putStrLn . ("  - " ++) . show) imports
          putStrLn $ "Executable statements: " ++ show (length executables)
          mapM_ (putStrLn . ("  - " ++) . show) executables

    ExitFailure code -> do
      putStrLn $ "cling-parser failed with exit code: " ++ show code
      putStrLn "stderr:"
      putStrLn stderr

printDeclaration :: CppDeclaration -> IO ()
printDeclaration CppDeclaration{..} = do
  let typeStr = case declType of
        UsingDirective -> "UsingDirective"
        Var -> "Var"
        Function -> "Function"
        CXXRecord -> "CXXRecord"
        TopLevelStmt -> "TopLevelStmt"
        Include -> "Include"
        Other t -> "Other(" ++ T.unpack t ++ ")"
  putStrLn $ "  " ++ typeStr ++ " at lines " ++
             maybe "?" show startLine ++ "-" ++ maybe "?" show endLine
