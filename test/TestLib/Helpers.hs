{-# LANGUAGE PackageImports #-}

module TestLib.Helpers where

import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich


shouldContainAny :: MonadIO m => [T.Text] -> [T.Text] -> m ()
shouldContainAny haystack needles =
  if any (`elem` haystack) needles
    then return ()
    else expectationFailure [i|Expected #{haystack} to contain at least one of #{needles}|]

shouldBeAtLeast :: MonadIO m => Int -> Int -> m ()
shouldBeAtLeast actual expected =
  if actual >= expected
    then return ()
    else expectationFailure [i|Expected at least #{expected} but got #{actual}|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]
