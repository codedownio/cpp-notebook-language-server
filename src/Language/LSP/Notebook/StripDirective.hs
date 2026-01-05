{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.StripDirective where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Sequence hiding (zip)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


data StripDirective = StripDirective Bool (Set.Set UInt)
  deriving (Show)

data SDParams = SDParams { sdParamsEnable :: Bool }
  deriving (Show)

instance Transformer StripDirective where
  type Params StripDirective = SDParams

  getParams (StripDirective enable _) = SDParams enable

  project :: MonadIO m => Params StripDirective -> Doc -> m (Doc, StripDirective)
  project (SDParams False) doc = return (doc, StripDirective False mempty)
  project (SDParams True) (docToList -> ls) = return $ go mempty mempty (zip ls [0..])
    where
      go affectedLines processedLines [] = (listToDoc (toList processedLines), StripDirective True affectedLines)
      go affectedLines processedLines ((line, n):rest)
        | isDirectiveLine line = go (Set.insert n affectedLines) (processedLines |> "") rest
        | otherwise = go affectedLines (processedLines |> line) rest

  transformPosition :: Params StripDirective -> StripDirective -> Position -> Maybe Position
  transformPosition (SDParams _) (StripDirective _ affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c

  untransformPosition :: Params StripDirective -> StripDirective -> Position -> Maybe Position
  untransformPosition (SDParams _) (StripDirective _ affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c


isDirectiveLine :: Text -> Bool
isDirectiveLine t = ":" `T.isPrefixOf` t
