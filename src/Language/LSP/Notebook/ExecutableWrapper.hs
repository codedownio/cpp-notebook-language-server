{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Language.LSP.Notebook.ExecutableWrapper where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Notebook.CppParser
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import UnliftIO.Process
import System.Exit (ExitCode(..))


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
        nonEmptyLines = filter (not . T.null . T.strip) originalLines
    if null nonEmptyLines
      then return (doc, ExecutableWrapper False 0 0 0)
      else do
        let wrappedDoc = wrapAllInSingleFunction params nonEmptyLines
            lineCount = length nonEmptyLines
        return (listToDoc wrappedDoc, ExecutableWrapper True 0 (lineCount + 1) lineCount)

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

-- Get lines that should NOT be sifted (everything else gets wrapped)
getNonSiftedLines :: [CppDeclaration] -> [Text] -> [Text]
getNonSiftedLines declarations originalLines = 
  let siftedLineNums = [ fromIntegral (line - 1) 
                       | decl <- declarations
                       , shouldBeSifted decl
                       , Just line <- [startLine decl]
                       ]
      indexedLines = zip ([0..] :: [Int]) originalLines
      nonSiftedLines = [ lineText 
                       | (lineNum, lineText) <- indexedLines
                       , lineNum `notElem` siftedLineNums
                       , not (T.strip lineText == "")  -- Skip empty lines
                       ]
  in nonSiftedLines

-- Check if declaration should be sifted (same logic as DeclarationSifter)
shouldBeSifted :: CppDeclaration -> Bool
shouldBeSifted CppDeclaration{declType = Include} = True
shouldBeSifted CppDeclaration{declType = UsingDirective} = True  
shouldBeSifted CppDeclaration{declType = CXXRecord} = True
shouldBeSifted CppDeclaration{declType = Function} = True
shouldBeSifted CppDeclaration{declType = Var} = True
shouldBeSifted _ = False

-- Wrap all lines in a single function (used after DeclarationSifter)
wrapAllInSingleFunction :: ExecutableWrapperParams -> [Text] -> [Text]
wrapAllInSingleFunction ExecutableWrapperParams{functionName} linesToWrap =
  let functionStart = "void " <> functionName <> "() {"
      functionEnd = "}"
      indentedBody = map ("  " <>) linesToWrap
  in [functionStart] ++ indentedBody ++ [functionEnd]

-- Wrap non-sifted lines in a single function
wrapInSingleFunction :: ExecutableWrapperParams -> [Text] -> [Text] -> [Text]
wrapInSingleFunction ExecutableWrapperParams{functionName} nonSiftedLines originalLines =
  let siftedLines = filter (`notElem` nonSiftedLines) originalLines
      functionStart = "void " <> functionName <> "() {"
      functionEnd = "}"
      indentedBody = map ("  " <>) nonSiftedLines
  in siftedLines ++ [functionStart] ++ indentedBody ++ [functionEnd]
