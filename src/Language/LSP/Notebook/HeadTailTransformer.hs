{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.HeadTailTransformer where

import Control.Monad.IO.Class ()
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


data HeadTailTransformer = HeadTailTransformer [Text] [Text] UInt UInt UInt
  deriving (Show, Eq)

instance Transformer HeadTailTransformer where
  type Params HeadTailTransformer = ([Text], [Text])

  getParams (HeadTailTransformer initialLines finalLines _ _ _) = (initialLines, finalLines)

  project (initialLines, finalLines) ls = return (beginning <> ls <> ending, tx)
    where
      beginning = case initialLines of
        [] -> ""
        xs -> joinLines xs <> "\n"

      ending = case finalLines of
        [] -> ""
        xs -> "\n" <> joinLines xs

      joinLines = Rope.fromText . T.intercalate "\n"

      tx = HeadTailTransformer initialLines
                               finalLines
                               (fromIntegral (L.length initialLines))
                               (fromIntegral (lengthInLines ls))
                               (fromIntegral (L.length finalLines))

  transformPosition _ (HeadTailTransformer _ _ s _d _e) (Position l c) = Just $ Position (l + s) c

  untransformPosition _ (HeadTailTransformer _ _ s d _e) (Position l c)
    | l < s = Nothing
    | l >= s + d = Nothing
    | otherwise = Just $ Position (l - s) c


lengthInLines :: Rope.Rope -> Word
lengthInLines rp | Rope.null rp = 0
lengthInLines rp = (+ 1) $ Rope.posLine $ Rope.lengthAsPosition rp
