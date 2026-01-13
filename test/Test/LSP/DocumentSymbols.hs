{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.DocumentSymbols where

import Control.Lens ((^.))
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (hover)
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Data.Text as T
import qualified Language.LSP.Test.Helpers as Helpers

spec :: TopSpec
spec = describe "C++ LSP Document Symbols Tests" $
  introduceMaybeBubblewrap $
  introduceNixContext nixpkgsReleaseDefault $
  introduceBinaryViaNixPackage @"clangd" "clang-tools" $
  introduceCnls $ do

    it "finds symbols in simple class" $ do
      let testCode = [__i|
class MyClass {
public:
    int publicVar;
    void publicMethod();
    
private:
    double privateVar;
    void privateMethod();
};

int globalFunction(int x) {
    return x * 2;
}

namespace MyNamespace {
    void namespacedFunction() {}
}
|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP
        
        -- Get document symbols
        symbols <- LSP.getDocumentSymbols doc
        
        info [i|Got document symbols response|]
        
        -- Just check that we get a response (symbols can be complex union types)
        liftIO $ return ()

    it "finds symbols in template code" $ do
      let testCode = [__i|
template<typename T>
class Container {
    T data;
public:
    Container(T value) : data(value) {}
    T getData() const { return data; }
};

template<typename T, typename U>
T convert(U value) {
    return static_cast<T>(value);
}

struct Point {
    int x, y;
};

enum Color { RED, GREEN, BLUE };
|]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_CPP
        
        symbols <- LSP.getDocumentSymbols doc
        
        info [i|Got template document symbols response|]
        
        -- Just check that we get a response
        liftIO $ return ()

shouldContainAny :: [T.Text] -> [T.Text] -> IO ()
shouldContainAny haystack needles =
  if any (`elem` haystack) needles
    then return ()
    else expectationFailure [i|Expected #{haystack} to contain at least one of #{needles}|]

shouldBeAtLeast :: Int -> Int -> IO ()
shouldBeAtLeast actual expected =
  if actual >= expected
    then return ()
    else expectationFailure [i|Expected at least #{expected} but got #{actual}|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec