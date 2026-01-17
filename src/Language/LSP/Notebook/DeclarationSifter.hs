{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.DeclarationSifter (
  DeclarationSifter
  , DeclarationSifterParams(..)
  ) where

import Control.Monad.IO.Class
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.LSP.Notebook.CppParser
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import System.Exit (ExitCode(..))
import UnliftIO.Process


data DeclarationSifter = DeclarationSifter {
  siftedIndices :: Vector Int         -- Original line indices in OUTPUT order (for untransform)
  , siftedIndicesSorted :: Vector Int -- Sorted for binary search (for transform count-less-than)
  , siftedMap :: Map Int Int          -- orig line → output position (for transform of sifted lines)
  , nonSiftedIndices :: Vector Int    -- Non-sifted original line indices in order (for untransform)
  , hasExecutableWrapper :: Bool
  , wrapperStartLine :: Int
  , wrapperEndLine :: Int
  } deriving Show

data DeclarationSifterParams = DeclarationSifterParams {
  parserCommand :: FilePath
  , execFunctionName :: Text  -- Name of the wrapper function (e.g. "__notebook_exec")
  } deriving Show

instance Transformer DeclarationSifter where
  type Params DeclarationSifter = DeclarationSifterParams

  getParams _ = DeclarationSifterParams "cling-parser" "__notebook_exec"

  project :: MonadIO m => Params DeclarationSifter -> Doc -> m (Doc, DeclarationSifter)
  project params doc = do
    result <- parseCppCode params doc
    let originalLines = docToList doc
    case result of
      Left _err -> return (doc, mkEmptySifter) -- fallback to no transformation
      Right declarations -> do
        let (siftedLineTexts, remainingLineTexts, siftedIdxs, nonSiftedIdxs) = siftDeclarations originalLines declarations

        -- Always wrap remaining executable lines (if any exist)
        if not (null remainingLineTexts) && any (not . T.null . T.strip) remainingLineTexts
          then do
            let wrappedLines = wrapInFunction (execFunctionName params) remainingLineTexts
                allLines = siftedLineTexts ++ wrappedLines
                wrapperStart = length siftedLineTexts
                wrapperEnd = length allLines - 1
            return (listToDoc allLines, mkSifter siftedIdxs nonSiftedIdxs True wrapperStart wrapperEnd)
          else
            return (listToDoc (siftedLineTexts ++ remainingLineTexts), mkSifter siftedIdxs nonSiftedIdxs False 0 0)

  transformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  transformPosition _ sifter pos = do
    Position l c <- transformUsingIndices sifter pos
    case hasExecutableWrapper sifter && l >= fromIntegral (wrapperStartLine sifter) of
      True -> Just $ Position (l + 2) (c + 2)  -- Account for wrapper (empty line + function header) AND indentation
      False -> Just $ Position l c

  untransformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  untransformPosition _ sifter (Position l c) =
    let unwrappedPos = if hasExecutableWrapper sifter && l > fromIntegral (wrapperStartLine sifter) && l <= fromIntegral (wrapperEndLine sifter)
                       then Position (l - 2) (if c >= 2 then c - 2 else 0)
                       else Position l c
    in untransformUsingIndices sifter unwrappedPos

mkSifter :: [Int] -> [Int] -> Bool -> Int -> Int -> DeclarationSifter
mkSifter siftedIdxs nonSiftedIdxs hasWrapper wStart wEnd = DeclarationSifter {
  siftedIndices = V.fromList siftedIdxs
  , siftedIndicesSorted = V.fromList (sort siftedIdxs)
  , siftedMap = Map.fromList [(idx, pos) | (pos, idx) <- zip [0..] siftedIdxs]
  , nonSiftedIndices = V.fromList nonSiftedIdxs
  , hasExecutableWrapper = hasWrapper
  , wrapperStartLine = wStart
  , wrapperEndLine = wEnd
  }

mkEmptySifter :: DeclarationSifter
mkEmptySifter = DeclarationSifter V.empty V.empty Map.empty V.empty False 0 0

parseCppCode :: MonadIO m => DeclarationSifterParams -> Doc -> m (Either String [CppDeclaration])
parseCppCode (DeclarationSifterParams {parserCommand}) doc = do
  let input = T.unpack (docToText doc)
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc parserCommand []) input
  case exitCode of
    ExitSuccess -> return $ parseCppDeclarations (T.pack stdout)
    ExitFailure _ -> return $ Left ("cling-parser failed: " ++ stderr)
  where
    docToText :: Doc -> Text
    docToText = T.intercalate "\n" . docToList

-- Sift declarations to the top in the right order
-- Returns: (siftedLines, remainingLines, siftedIndices, nonSiftedIndices)
siftDeclarations :: [Text] -> [CppDeclaration] -> ([Text], [Text], [Int], [Int])
siftDeclarations originalLines declarations = (siftedLines, remainingLines, allSiftedIndices, nonSiftedIndices)
  where
    declRanges = [(declType decl, getLineRange decl) | decl <- declarations, isValidRange decl]

    -- Group by declaration type in desired order
    includeRanges = [range | (Include, range) <- declRanges]
    usingRanges = [range | (UsingDirective, range) <- declRanges]
    classRanges = [range | (CXXRecord, range) <- declRanges]
    functionRanges = [range | (Function, range) <- declRanges]
    varRanges = [range | (Var, range) <- declRanges]

    -- Get all sifted line indices (0-based) - in OUTPUT order (not sorted!)
    -- This order must match siftedLines for position transformation to work
    allSiftedIndices = concatMap rangeToIndices (includeRanges ++ usingRanges ++ classRanges ++ functionRanges ++ varRanges)
    siftedSet = Set.fromList allSiftedIndices

    -- Extract lines by declaration type groups (maintain proper order)
    indexedLines = zip [0..] originalLines

    -- Extract lines for each group separately to maintain order
    includeLines = concatMap (extractLinesForRange indexedLines) includeRanges
    usingLines = concatMap (extractLinesForRange indexedLines) usingRanges
    classLines = concatMap (extractLinesForRange indexedLines) classRanges
    functionLines = concatMap (extractLinesForRange indexedLines) functionRanges
    varLines = concatMap (extractLinesForRange indexedLines) varRanges

    siftedLines = includeLines ++ usingLines ++ classLines ++ functionLines ++ varLines
    -- Non-sifted lines and their indices (in original order)
    nonSiftedWithIdx = [(idx, line) | (idx, line) <- indexedLines, idx `Set.notMember` siftedSet]
    remainingLines = map snd nonSiftedWithIdx
    nonSiftedIndices = map fst nonSiftedWithIdx

    getLineRange :: CppDeclaration -> (Int, Int)
    getLineRange (CppDeclaration {startLine = Just start, endLine = Just end}) = (start, end)
    getLineRange (CppDeclaration {startLine = Just start, endLine = Nothing}) = (start, start)
    getLineRange _ = (0, 0)

    isValidRange :: CppDeclaration -> Bool
    isValidRange (CppDeclaration {startLine = Just _, endLine = Just _}) = True
    isValidRange (CppDeclaration {startLine = Just _, endLine = Nothing}) = True  -- Single line
    isValidRange _ = False

    rangeToIndices :: (Int, Int) -> [Int]
    rangeToIndices (start, end) = [start - 1 .. end - 1]  -- Convert to 0-based indexing

    extractLinesForRange :: [(Int, Text)] -> (Int, Int) -> [Text]
    extractLinesForRange indexedLines (start, end) =
      let targetIndices = [start - 1 .. end - 1]  -- Convert to 0-based
      in [line | (idx, line) <- indexedLines, idx `elem` targetIndices]

transformUsingIndices :: DeclarationSifter -> Position -> Maybe Position
transformUsingIndices sifter (Position l c) =
  let origLine = fromIntegral l
  in case Map.lookup origLine (siftedMap sifter) of
       Just outputPos -> Just (Position (fromIntegral outputPos) c)
       Nothing ->
         -- Line is not sifted, use binary search to count sifted lines before it
         let siftedBefore = binarySearchCountLessThan (siftedIndicesSorted sifter) origLine
             nonSiftedBefore = origLine - siftedBefore
         in Just (Position (fromIntegral (V.length (siftedIndices sifter) + nonSiftedBefore)) c)

untransformUsingIndices :: DeclarationSifter -> Position -> Maybe Position
untransformUsingIndices sifter (Position l c)
  -- Empty sifter = identity transformation (no change)
  | V.null (siftedIndices sifter) && V.null (nonSiftedIndices sifter) = Just (Position l c)
  | l < fromIntegral numSifted = Just $ Position (fromIntegral (siftedIndices sifter V.! fromIntegral l)) c
  | remainingIdx < V.length (nonSiftedIndices sifter) = Just $ Position (fromIntegral (nonSiftedIndices sifter V.! remainingIdx)) c
  | otherwise = Nothing  -- Synthetic wrapper line (empty line, header, closing brace)
  where
    numSifted = V.length (siftedIndices sifter)
    remainingIdx = fromIntegral l - numSifted

binarySearchCountLessThan :: Vector Int -> Int -> Int
binarySearchCountLessThan vec target = go 0 (V.length vec)
  where
    go lb ub
      | lb == ub = lb
      | otherwise =
          let mid = (lb + ub) `div` 2
              midVal = vec V.! mid
          in if midVal < target
             then go (mid + 1) ub
             else go lb mid

wrapInFunction :: Text -> [Text] -> [Text]
wrapInFunction functionName executableLines =
  let functionStart = "void " <> functionName <> "() {"
      functionEnd = "}"
      -- Filter out empty lines at the beginning and end
      trimmedLines = dropWhile (T.null . T.strip) $ reverse $ dropWhile (T.null . T.strip) $ reverse executableLines
      indentedBody = map (\l -> if T.null l then l else "  " <> l) trimmedLines
  in if null trimmedLines
     then []  -- No executable lines to wrap
     else [""] ++ [functionStart] ++ indentedBody ++ [functionEnd]
