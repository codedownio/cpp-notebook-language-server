{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Language.LSP.Notebook.ImportSifter where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Language.LSP.Notebook.CppParser
import UnliftIO.Process
import System.Exit (ExitCode(..))

data ImportSifter = ImportSifter (Vector Int)
  deriving Show

data ImportSifterParams = ImportSifterParams
  { parserCommand :: FilePath
  } deriving Show

instance Transformer ImportSifter where
  type Params ImportSifter = ImportSifterParams

  getParams (ImportSifter _) = ImportSifterParams "cling-parser"

  project :: MonadIO m => Params ImportSifter -> Doc -> m (Doc, ImportSifter)
  project params doc = do
    result <- parseCppCode params doc
    case result of
      Left _err -> return (doc, ImportSifter V.empty) -- fallback to no transformation
      Right declarations -> do
        let importLines = getImportLines declarations
            indices = V.fromList importLines
            (importLs, otherLs) = partitionLines importLines (zip [0..] (docToList doc))
        return (listToDoc (importLs <> otherLs), ImportSifter indices)

  transformPosition :: Params ImportSifter -> ImportSifter -> Position -> Maybe Position
  transformPosition _ (ImportSifter indices) = transformUsingIndices indices

  untransformPosition :: Params ImportSifter -> ImportSifter -> Position -> Maybe Position  
  untransformPosition _ (ImportSifter indices) = Just . untransformUsingIndices indices

parseCppCode :: MonadIO m => ImportSifterParams -> Doc -> m (Either String [CppDeclaration])
parseCppCode ImportSifterParams{parserCommand} doc = do
  let input = T.unpack (docToText doc)
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc parserCommand []) input
  case exitCode of
    ExitSuccess -> return $ parseCppDeclarations (T.pack stdout)
    ExitFailure _ -> return $ Left ("cling-parser failed: " ++ stderr)

docToText :: Doc -> Text  
docToText = T.intercalate "\n" . docToList

getImportLines :: [CppDeclaration] -> [Int]
getImportLines decls = 
  [ line - 1 -- Convert to 0-based indexing
  | decl <- decls
  , isImportLike decl
  , Just line <- [startLine decl]
  ]

partitionLines :: [Int] -> [(Int, Text)] -> ([Text], [Text])
partitionLines [] remaining = ([], fmap snd remaining)
partitionLines _ [] = ([], [])
partitionLines all@(nextDesired:xs) ((curIndex, curLine):ys)
  | nextDesired == curIndex = 
      let (chosen, notChosen) = partitionLines xs ys 
      in (curLine : chosen, notChosen)
  | otherwise = 
      let (chosen, notChosen) = partitionLines all ys
      in (chosen, curLine : notChosen)

transformUsingIndices :: Vector Int -> Position -> Maybe Position
transformUsingIndices indices (Position l c) = 
  case binarySearch indices (fromIntegral l) of
    (i, True) -> Just (Position (fromIntegral i) c)
    (i, False) -> Just (Position (l + fromIntegral (V.length indices - i)) c)

untransformUsingIndices :: Vector Int -> Position -> Position  
untransformUsingIndices indices (Position l c)
  | l < fromIntegral (V.length indices) = 
      Position (fromIntegral (indices V.! fromIntegral l)) c
  | otherwise = Position finalL c
  where
    finalL = case V.length indices of
      0 -> l
      len -> l + fromIntegral (indices V.! (len - 1))

binarySearch :: Vector Int -> Int -> (Int, Bool)
binarySearch vec target = go 0 (V.length vec)
  where
    go lb ub 
      | lb == ub = (lb, False)
      | otherwise = 
          let mid = (lb + ub) `div` 2
              midVal = vec V.! mid
          in case midVal `compare` target of
               LT -> go (mid + 1) ub
               EQ -> (mid, True)  
               GT -> go lb mid