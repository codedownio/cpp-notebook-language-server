
module TestLib.Common where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich


transformAndUntransform :: (MonadThrow m, MonadIO m, Transformer a) => Params a -> Position -> Position -> a -> m ()
transformAndUntransform params from to x = do
  transformPosition params x from `shouldBe` (Just to)
  untransformPosition params x to `shouldBe` (Just from)
