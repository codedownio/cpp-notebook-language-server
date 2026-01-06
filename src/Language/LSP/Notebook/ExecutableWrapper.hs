{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Language.LSP.Notebook.ExecutableWrapper where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Notebook.CppParser
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import System.Exit (ExitCode(..))
import UnliftIO.Process


data ExecutableWrapper = ExecutableWrapper {
  hasWrapper :: Bool
  , wrapperStartLine :: Int  -- Where we inserted function start
  , wrapperEndLine :: Int    -- Where we inserted function end
  , wrappedLineCount :: Int  -- Number of lines wrapped
  } deriving Show

data ExecutableWrapperParams = ExecutableWrapperParams {
  functionName :: Text
  , parserCmd :: FilePath
  } deriving Show

instance Transformer ExecutableWrapper where
  type Params ExecutableWrapper = ExecutableWrapperParams

  getParams (ExecutableWrapper _ _ _ _) = ExecutableWrapperParams
    "__notebook_exec"
    "minimal-parser"

  project :: MonadIO m => Params ExecutableWrapper -> Doc -> m (Doc, ExecutableWrapper)
  project params doc = do
    let originalLines = docToList doc

    -- Parse the already-sifted code to identify what are declarations vs executable statements
    parseResult <- parseCppCodeForExec params doc
    case parseResult of
      Left err -> error $ "ExecutableWrapper parse failed: " ++ err
      Right declarations -> do
        -- Get lines that are NOT declarations (these are the executable statements)
        let executableLines = getExecutableLines declarations originalLines

        if null executableLines
          then return (doc, ExecutableWrapper False 0 0 0)
          else do
            -- Separate declarations (keep at top) from executable statements (wrap in function)
            let (declarationLines, _) = separateDeclarationsFromExecutables declarations originalLines
                wrappedLines = declarationLines ++ wrapInFunction params executableLines
                wrapperStartIdx = length declarationLines
                wrapperEndIdx = wrapperStartIdx + length executableLines + 1  -- +1 for function closing brace
            return (listToDoc wrappedLines, ExecutableWrapper True wrapperStartIdx wrapperEndIdx (length executableLines))

  transformPosition :: Params ExecutableWrapper -> ExecutableWrapper -> Position -> Maybe Position
  transformPosition _ (ExecutableWrapper False _ _ _) p = Just p
  transformPosition _ (ExecutableWrapper True startLine endLine _) (Position l c)
    | l >= fromIntegral startLine && l <= fromIntegral endLine = Just $ Position (l + 1) c  -- Inside wrapper
    | l > fromIntegral endLine = Just $ Position (l + 2) c  -- After wrapper (function start + end)
    | otherwise = Just $ Position l c

  untransformPosition :: Params ExecutableWrapper -> ExecutableWrapper -> Position -> Maybe Position
  untransformPosition _ (ExecutableWrapper False _ _ _) p = Just p
  untransformPosition _ (ExecutableWrapper True startLine endLine _) (Position l c)
    | l <= fromIntegral startLine = Just $ Position l c  -- Before wrapper
    | l > fromIntegral endLine + 1 = Just $ Position (l - 2) c  -- After wrapper
    | otherwise = Just $ Position (l - 1) c  -- Inside wrapper

parseCppCodeForExec :: MonadIO m => ExecutableWrapperParams -> Doc -> m (Either String [CppDeclaration])
parseCppCodeForExec ExecutableWrapperParams{parserCmd} doc = do
  let input = T.unpack (T.intercalate "\n" (docToList doc))
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc parserCmd []) input
  case exitCode of
    ExitSuccess -> return $ parseCppDeclarations (T.pack stdout)
    ExitFailure _ -> return $ Left ("minimal-parser failed: " ++ stderr)

-- Get lines that are executable statements (not declarations) - need to be wrapped in function
getExecutableLines :: [CppDeclaration] -> [Text] -> [Text]
getExecutableLines declarations originalLines =
  let declLineNums = [ fromIntegral (line - 1)
                     | decl <- declarations
                     , isDeclaration decl
                     , Just line <- [startLine decl]
                     ]
      indexedLines = zip ([0..] :: [Int]) originalLines
      executableLines = [ lineText
                        | (lineNum, lineText) <- indexedLines
                        , lineNum `notElem` declLineNums
                        , not (T.strip lineText == "")  -- Skip empty lines
                        ]
  in executableLines

-- Separate declaration lines from executable lines, returning both lists
separateDeclarationsFromExecutables :: [CppDeclaration] -> [Text] -> ([Text], [Int])
separateDeclarationsFromExecutables declarations originalLines =
  let declLineNums = [ fromIntegral (line - 1)
                     | decl <- declarations
                     , isDeclaration decl
                     , Just line <- [startLine decl]
                     ]
      indexedLines = zip ([0..] :: [Int]) originalLines
      (declLines, execIndices) = foldl categorize ([], []) indexedLines

      categorize (decls, execs) (lineNum, lineText) =
        if lineNum `elem` declLineNums || T.strip lineText == ""
        then (decls ++ [lineText], execs)  -- Keep declarations and empty lines at top
        else (decls, execs ++ [lineNum])   -- Mark executable lines for wrapping

  in (declLines, execIndices)

-- Check if something is a declaration (vs executable statement)
isDeclaration :: CppDeclaration -> Bool
isDeclaration CppDeclaration{declType = Include} = True
isDeclaration CppDeclaration{declType = UsingDirective} = True
isDeclaration CppDeclaration{declType = CXXRecord} = True
isDeclaration CppDeclaration{declType = Function} = True
isDeclaration CppDeclaration{declType = Var} = True
isDeclaration CppDeclaration{declType = TopLevelStmt} = False  -- This is executable!
isDeclaration _ = True  -- Conservative default

-- Check if declaration should be sifted (same logic as DeclarationSifter)
shouldBeSifted :: CppDeclaration -> Bool
shouldBeSifted CppDeclaration{declType = Include} = True
shouldBeSifted CppDeclaration{declType = UsingDirective} = True
shouldBeSifted CppDeclaration{declType = CXXRecord} = True
shouldBeSifted CppDeclaration{declType = Function} = True
shouldBeSifted CppDeclaration{declType = Var} = True
shouldBeSifted _ = False

-- Wrap executable lines in a function (used in pipeline after DeclarationSifter)
wrapInFunction :: ExecutableWrapperParams -> [Text] -> [Text]
wrapInFunction ExecutableWrapperParams{functionName} executableLines =
  let functionStart = "void " <> functionName <> "() {"
      functionEnd = "}"
      indentedBody = map ("  " <>) executableLines
  in [functionStart] ++ indentedBody ++ [functionEnd]
