{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.DeclarationSifter (
  DeclarationSifter(..)
  , DeclarationSifterParams(..)
  ) where

import Control.Monad.IO.Class
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


-- | The transformation is essentially a permutation of lines.
-- We store both directions for O(1) transform and untransform.
data DeclarationSifter = DeclarationSifter {
  forward :: Vector Int           -- forward[origLine] = outputLine
  , inverse :: Vector Int         -- inverse[outputLine] = origLine (-1 for synthetic)
  , wrapperBodyStart :: Int       -- first output line of wrapper body (0 if no wrapper)
  , wrapperBodyEnd :: Int         -- last output line of wrapper body (0 if no wrapper)
  } deriving Show

data DeclarationSifterParams = DeclarationSifterParams {
  parserCommand :: FilePath
  , execFunctionName :: Text
  } deriving Show

instance Transformer DeclarationSifter where
  type Params DeclarationSifter = DeclarationSifterParams

  getParams _ = DeclarationSifterParams "cling-parser" "__notebook_exec"

  project :: MonadIO m => Params DeclarationSifter -> Doc -> m (Doc, DeclarationSifter, Either Text ())
  project params doc = do
    result <- parseCppCode params doc
    let originalLines = docToList doc
        numOrigLines = length originalLines
    case result of
      Left err -> return (doc, mkIdentitySifter numOrigLines, Left (T.pack err))
      Right declarations -> do
        let (siftedIdxs, nonSiftedIdxs) = partitionIndices originalLines declarations
            siftedLines = map (originalLines !!) siftedIdxs
            nonSiftedLines = map (originalLines !!) nonSiftedIdxs

        if not (null nonSiftedLines) && any (not . T.null . T.strip) nonSiftedLines
          then do
            -- Wrap executable lines in a function
            let wrappedLines = wrapInFunction (execFunctionName params) nonSiftedLines
                allLines = siftedLines ++ wrappedLines
                numSifted = length siftedIdxs
                -- Wrapper structure: header, body lines, closing brace
                bodyStart = numSifted + 1  -- after function header
                bodyEnd = numSifted + length nonSiftedLines  -- before closing brace
                sifter = mkSifter numOrigLines siftedIdxs nonSiftedIdxs (Just (bodyStart, bodyEnd))
            return (listToDoc allLines, sifter, Right ())
          else do
            -- No wrapper needed
            let allLines = siftedLines ++ nonSiftedLines
                sifter = mkSifter numOrigLines siftedIdxs nonSiftedIdxs Nothing
            return (listToDoc allLines, sifter, Right ())

  transformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  transformPosition _ sifter (Position l c)
    | origLine < 0 || origLine >= V.length (forward sifter) = Nothing
    | otherwise =
        let outputLine = forward sifter V.! origLine
            c' = if inWrapperBody sifter outputLine then c + 2 else c
        in Just (Position (fromIntegral outputLine) c')
    where
      origLine = fromIntegral l

  untransformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  untransformPosition _ sifter (Position l c)
    | outputLine < 0 || outputLine >= V.length (inverse sifter) = Nothing
    | origLine < 0 = Nothing  -- synthetic line
    | otherwise =
        let c' = if inWrapperBody sifter outputLine
                 then if c >= 2 then c - 2 else 0
                 else c
        in Just (Position (fromIntegral origLine) c')
    where
      outputLine = fromIntegral l
      origLine = inverse sifter V.! outputLine

-- | Check if an output line is in the wrapper body (needs column adjustment)
inWrapperBody :: DeclarationSifter -> Int -> Bool
inWrapperBody sifter outputLine =
  wrapperBodyStart sifter > 0 &&
  outputLine >= wrapperBodyStart sifter &&
  outputLine <= wrapperBodyEnd sifter

-- | Create identity sifter (no transformation)
mkIdentitySifter :: Int -> DeclarationSifter
mkIdentitySifter n = DeclarationSifter
  { forward = V.fromList [0..n-1]
  , inverse = V.fromList [0..n-1]
  , wrapperBodyStart = 0
  , wrapperBodyEnd = 0
  }

-- | Build the permutation vectors from sifted and non-sifted indices
mkSifter :: Int -> [Int] -> [Int] -> Maybe (Int, Int) -> DeclarationSifter
mkSifter numOrigLines siftedIdxs nonSiftedIdxs mWrapperBody =
  DeclarationSifter
    { forward = V.fromList fwdList
    , inverse = V.fromList invList
    , wrapperBodyStart = maybe 0 fst mWrapperBody
    , wrapperBodyEnd = maybe 0 snd mWrapperBody
    }
  where
    numSifted = length siftedIdxs
    hasWrapper = maybe False (const True) mWrapperBody

    -- Build forward: origLine -> outputLine
    fwdList = map toOutput [0..numOrigLines-1]
    toOutput origLine =
      case lookup origLine siftedWithOutputPos of
        Just outPos -> outPos
        Nothing -> case lookup origLine nonSiftedWithOutputPos of
          Just outPos -> outPos
          Nothing -> -1  -- shouldn't happen

    siftedWithOutputPos = zip siftedIdxs [0..]  -- (origLine, outputPos)
    nonSiftedWithOutputPos =
      if hasWrapper
      then zip nonSiftedIdxs [numSifted + 1..]  -- skip header line
      else zip nonSiftedIdxs [numSifted..]

    -- Build inverse: outputLine -> origLine (-1 for synthetic)
    invList = siftedOrig ++ wrapperInv ++ nonSiftedOrig ++ closingInv
    siftedOrig = siftedIdxs
    (wrapperInv, nonSiftedOrig, closingInv) =
      if hasWrapper
      then ([-1], nonSiftedIdxs, [-1])  -- header, body, closing brace
      else ([], nonSiftedIdxs, [])

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

-- | Partition line indices into sifted (declarations) and non-sifted (executable)
-- Returns (siftedIndices, nonSiftedIndices) where siftedIndices are in output order
partitionIndices :: [Text] -> [CppDeclaration] -> ([Int], [Int])
partitionIndices originalLines declarations = (allSiftedIndices, nonSiftedIndices)
  where
    declRanges = [(declType decl, getLineRange decl) | decl <- declarations, isValidRange decl]

    -- Group by declaration type in desired order
    includeRanges = [range | (Include, range) <- declRanges]
    usingRanges = [range | (UsingDirective, range) <- declRanges]
    classRanges = [range | (CXXRecord, range) <- declRanges]
    functionRanges = [range | (Function, range) <- declRanges]
    varRanges = [range | (Var, range) <- declRanges]

    -- Sifted indices in OUTPUT order (includes, using, classes, functions, vars)
    allSiftedIndices = concatMap rangeToIndices (includeRanges ++ usingRanges ++ classRanges ++ functionRanges ++ varRanges)
    siftedSet = Set.fromList allSiftedIndices

    -- Non-sifted indices in original order
    nonSiftedIndices = [i | i <- [0..length originalLines - 1], i `Set.notMember` siftedSet]

    getLineRange :: CppDeclaration -> (Int, Int)
    getLineRange (CppDeclaration {startLine = Just start, endLine = Just end}) = (start, end)
    getLineRange (CppDeclaration {startLine = Just start, endLine = Nothing}) = (start, start)
    getLineRange _ = (0, 0)

    isValidRange :: CppDeclaration -> Bool
    isValidRange (CppDeclaration {startLine = Just _, endLine = Just _}) = True
    isValidRange (CppDeclaration {startLine = Just _, endLine = Nothing}) = True
    isValidRange _ = False

    rangeToIndices :: (Int, Int) -> [Int]
    rangeToIndices (start, end) = [start - 1 .. end - 1]  -- Convert to 0-based

-- | Wrap executable lines in a function (no empty line prefix)
wrapInFunction :: Text -> [Text] -> [Text]
wrapInFunction functionName executableLines =
  if null trimmedLines
  then []
  else [functionStart] ++ indentedBody ++ [functionEnd]
  where
    functionStart = "void " <> functionName <> "() {"
    functionEnd = "}"
    trimmedLines = dropWhile (T.null . T.strip) $ reverse $ dropWhile (T.null . T.strip) $ reverse executableLines
    indentedBody = map (\l -> if T.null l then l else "  " <> l) trimmedLines
