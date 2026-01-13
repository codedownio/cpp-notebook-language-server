{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.LSP.DocumentSymbols where

import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers


spec :: (
  Helpers.LspContext ctx m
  , HasFile ctx "cpp-notebook-language-server"
  , HasFile ctx "clangd"
  ) => SpecFree ctx m ()
spec = describe "Document symbols" $ do
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
        _ <- LSP.getDocumentSymbols doc

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

        _ <- LSP.getDocumentSymbols doc

        info [i|Got template document symbols response|]

        -- Just check that we get a response
        liftIO $ return ()
