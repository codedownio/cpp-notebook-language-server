
import Test.Sandwich

import qualified Test.Hover
import qualified Test.Pipeline

import qualified Test.Transformer.DeclarationSifter
import qualified Test.LSP.Hover


spec :: TopSpec
spec = do
  Test.Hover.spec

  Test.Transformer.DeclarationSifter.spec

  Test.Pipeline.spec

  Test.LSP.Hover.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
