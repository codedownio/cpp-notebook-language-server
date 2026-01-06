
import Test.Sandwich

import qualified Test.Hover
import qualified Test.Pipeline

import qualified Test.Transformer.DeclarationSifter


spec :: TopSpec
spec = do
  Test.Hover.spec

  Test.Transformer.DeclarationSifter.spec

  Test.Pipeline.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
