
import Test.Sandwich

import qualified Test.Hover
import qualified Test.Pipeline

import qualified Test.Transformer.DeclarationSifter
import qualified Test.LSP.Hover
import qualified Test.LSP.Completions
import qualified Test.LSP.DocumentSymbols


spec :: TopSpec
spec = do
  Test.Hover.spec
  Test.Transformer.DeclarationSifter.spec
  Test.Pipeline.spec

  Test.LSP.Hover.spec
  Test.LSP.Completions.spec
  Test.LSP.DocumentSymbols.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
