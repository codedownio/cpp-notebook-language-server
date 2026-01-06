{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Language.LSP.Notebook.DeclarationSifter where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.LSP.Notebook.CppParser
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import System.Exit (ExitCode(..))
import UnliftIO.Process

data DeclarationSifter = DeclarationSifter
  { siftedLines :: Vector Int  -- Lines that were moved to the top
  , hasExecutableWrapper :: Bool  -- Whether we wrapped executable statements
  , wrapperStartLine :: Int  -- Where the wrapper function starts (after sifting)
  , wrapperEndLine :: Int  -- Where the wrapper function ends
  } deriving Show

data DeclarationSifterParams = DeclarationSifterParams
  { parserCommand :: FilePath
  , execFunctionName :: Text  -- Name of the wrapper function (e.g. "__notebook_exec")
  } deriving Show

instance Transformer DeclarationSifter where
  type Params DeclarationSifter = DeclarationSifterParams

  getParams (DeclarationSifter _ _ _ _) = DeclarationSifterParams "minimal-parser" "__notebook_exec"

  project :: MonadIO m => Params DeclarationSifter -> Doc -> m (Doc, DeclarationSifter)
  project params doc = do
    result <- parseCppCode params doc
    let originalLines = docToList doc
    case result of
      Left _err -> return (doc, DeclarationSifter V.empty False 0 0) -- fallback to no transformation
      Right declarations -> do
        let (siftedLines, remainingLines, siftedIndices) = siftDeclarations originalLines declarations

        -- Always wrap remaining executable lines (if any exist)
        if not (null remainingLines) && any (not . T.null . T.strip) remainingLines
          then do
            let wrappedLines = wrapInFunction (execFunctionName params) remainingLines
                allLines = siftedLines ++ wrappedLines
                wrapperStart = length siftedLines
                wrapperEnd = length allLines - 1  -- Last line of wrapped function
            return (listToDoc allLines, DeclarationSifter (V.fromList siftedIndices) True wrapperStart wrapperEnd)
          else
            return (listToDoc (siftedLines ++ remainingLines), DeclarationSifter (V.fromList siftedIndices) False 0 0)

  transformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  transformPosition _ (DeclarationSifter indices hasWrapper wStart _) pos =
    -- First apply sifting transformation
    case transformUsingIndices indices pos of
      Nothing -> Nothing
      Just (Position l c) ->
        -- Then handle wrapper transformation if applicable
        if hasWrapper && l >= fromIntegral wStart
        then Just $ Position (l + 1) c  -- Account for wrapper function start line
        else Just $ Position l c

  untransformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  untransformPosition _ (DeclarationSifter indices hasWrapper wStart wEnd) (Position l c) =
    -- First undo wrapper transformation if applicable
    let unwrappedPos = if hasWrapper && l > fromIntegral wStart && l <= fromIntegral wEnd
                       then Position (l - 1) c  -- Remove wrapper function indentation
                       else Position l c
    -- Then undo sifting transformation
    in Just $ untransformUsingIndices indices unwrappedPos

-- Parse C++ code using minimal-parser
parseCppCode :: MonadIO m => DeclarationSifterParams -> Doc -> m (Either String [CppDeclaration])
parseCppCode DeclarationSifterParams{parserCommand} doc = do
  let input = T.unpack (docToText doc)
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc parserCommand []) input
  case exitCode of
    ExitSuccess -> return $ parseCppDeclarations (T.pack stdout)
    ExitFailure _ -> return $ Left ("minimal-parser failed: " ++ stderr)

docToText :: Doc -> Text
docToText = T.intercalate "\n" . docToList

-- Sift declarations to the top in the right order
-- Returns: (siftedLines, remainingLines, siftedIndices)
-- Fixed version - preserves complete multi-line declarations
siftDeclarations :: [Text] -> [CppDeclaration] -> ([Text], [Text], [Int])
siftDeclarations originalLines declarations =
  let -- Extract line ranges for each declaration type
      declRanges = [(declType decl, getLineRange decl) | decl <- declarations, isValidRange decl]

      -- Group by declaration type in desired order
      includeRanges = [range | (Include, range) <- declRanges]
      usingRanges = [range | (UsingDirective, range) <- declRanges]
      classRanges = [range | (CXXRecord, range) <- declRanges]
      functionRanges = [range | (Function, range) <- declRanges]
      varRanges = [range | (Var, range) <- declRanges]

      -- Get all sifted line indices (0-based) - fixed order
      allSiftedIndices = concatMap rangeToIndices (includeRanges ++ usingRanges ++ classRanges ++ functionRanges ++ varRanges)

      -- Extract lines by declaration type groups (maintain proper order)
      indexedLines = zip [0..] originalLines

      -- Extract lines for each group separately to maintain order
      includeLines = concatMap (extractLinesForRange indexedLines) includeRanges
      usingLines = concatMap (extractLinesForRange indexedLines) usingRanges
      classLines = concatMap (extractLinesForRange indexedLines) classRanges
      functionLines = concatMap (extractLinesForRange indexedLines) functionRanges
      varLines = concatMap (extractLinesForRange indexedLines) varRanges

      siftedLines = includeLines ++ usingLines ++ classLines ++ functionLines ++ varLines
      remainingLines = [line | (idx, line) <- indexedLines, idx `notElem` allSiftedIndices]

  in (siftedLines, remainingLines, allSiftedIndices)

-- Helper functions for line range extraction
getLineRange :: CppDeclaration -> (Int, Int)
getLineRange CppDeclaration{startLine = Just start, endLine = Just end} = (start, end)
getLineRange CppDeclaration{startLine = Just start, endLine = Nothing} = (start, start)
getLineRange _ = (0, 0)

isValidRange :: CppDeclaration -> Bool
isValidRange CppDeclaration{startLine = Just _, endLine = Just _} = True
isValidRange CppDeclaration{startLine = Just _, endLine = Nothing} = True  -- Single line
isValidRange _ = False

rangeToIndices :: (Int, Int) -> [Int]
rangeToIndices (start, end) = [start - 1 .. end - 1]  -- Convert to 0-based indexing

extractLinesForRange :: [(Int, Text)] -> (Int, Int) -> [Text]
extractLinesForRange indexedLines (start, end) =
  let targetIndices = [start - 1 .. end - 1]  -- Convert to 0-based
  in [line | (idx, line) <- indexedLines, idx `elem` targetIndices]

data LineType = LineType_Include | LineType_Using | LineType_Class | LineType_Function | LineType_Var | LineType_Executable
  deriving (Eq, Show)

-- Classify a line based on patterns and minimal-parser declarations
classifyLine :: [CppDeclaration] -> (Int, Text) -> (Int, Text, LineType)
classifyLine declarations (idx, line) =
  let lineNum = idx + 1 -- Convert to 1-based for declaration matching
      stripped = T.strip line

      -- Check if minimal-parser identified this line
      declAtLine = [decl | decl <- declarations, Just l <- [startLine decl], fromIntegral l == lineNum]

      lineType = case declAtLine of
        [decl] -> case declType decl of
          UsingDirective -> LineType_Using
          CXXRecord -> LineType_Class
          Function -> LineType_Function
          Var -> LineType_Var
          _ -> classifyByPattern stripped
        _ -> classifyByPattern stripped

  in (idx, line, lineType)

-- Classify by text patterns (fallback when minimal-parser doesn't detect)
classifyByPattern :: Text -> LineType
classifyByPattern stripped
  | T.isPrefixOf "#include" stripped = LineType_Include
  | T.isPrefixOf "using namespace" stripped = LineType_Using
  | T.isPrefixOf "using " stripped = LineType_Using
  | T.isPrefixOf "class " stripped = LineType_Class
  | T.isPrefixOf "struct " stripped = LineType_Class
  | isGlobalVarPattern stripped = LineType_Var
  | isFunctionPattern stripped = LineType_Function
  | T.null stripped = LineType_Executable  -- Empty lines go with executable
  | T.isPrefixOf "//" stripped = LineType_Executable  -- Comments go with executable
  | otherwise = LineType_Executable

-- Simple heuristic for global variable declarations
isGlobalVarPattern :: Text -> Bool
isGlobalVarPattern line =
  let words = T.words line
  in length words >= 3 &&
     case words of
       (firstWord:_) -> firstWord `elem` ["int", "double", "float", "char", "bool", "string", "auto"] && "=" `elem` words
       [] -> False

-- Simple heuristic for function declarations
isFunctionPattern :: Text -> Bool
isFunctionPattern line =
  "(" `T.isInfixOf` line &&
  ")" `T.isInfixOf` line &&
  "{" `T.isInfixOf` line

-- Position transformation utilities
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

-- Wrap executable lines in a function
wrapInFunction :: Text -> [Text] -> [Text]
wrapInFunction functionName executableLines =
  let functionStart = "void " <> functionName <> "() {"
      functionEnd = "}"
      -- Filter out empty lines at the beginning and end
      trimmedLines = dropWhile (T.null . T.strip) $ reverse $ dropWhile (T.null . T.strip) $ reverse executableLines
      indentedBody = map ("  " <>) trimmedLines
  in if null trimmedLines
     then []  -- No executable lines to wrap
     else [""] ++ [functionStart] ++ indentedBody ++ [functionEnd]
