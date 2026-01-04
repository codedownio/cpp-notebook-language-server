{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens as Lens
import System.FilePath
import Transform.Util
import UnliftIO.Directory
import UnliftIO.MVar


-- * Transform

transformRange :: CppNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => CppNotebookTransformer -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition (getParams tx) tx)
  >>= traverseOf (range . end) (transformPosition (getParams tx) tx)

-- * Untransform

untransformRange :: CppNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => CppNotebookTransformer -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition (getParams tx) tx)
  >>= traverseOf (range . end) (untransformPosition (getParams tx) tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => CppNotebookTransformer -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition (getParams tx) tx)
  >>= traverseOf (range . _Just . end) (untransformPosition (getParams tx) tx)

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id

-- * Update the shadow lib.rs file

updateLibRs :: TransformerMonad m => m ()
updateLibRs = do
  TransformerState {..} <- ask
  docs <- readMVar transformerDocuments
  let identifiers = [dropExtension (takeFileName newPath) | DocumentState {..} <- fmap snd (M.toList docs)]
  let libRsContent = T.intercalate "\n" [T.pack ("mod " <> x <> ";") | x <- identifiers]
  logInfoN [i|libRsContent: #{libRsContent}|]
  liftIO $ createDirectoryIfMissing True (transformerShadowDir </> "src")
  liftIO $ T.writeFile (transformerShadowDir </> "src" </> "lib.rs") libRsContent
