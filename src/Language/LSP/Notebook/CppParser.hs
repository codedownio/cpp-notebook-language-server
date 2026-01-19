{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.CppParser where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics

data CppDeclType =
  UsingDirective
  | Var
  | Function
  | CXXRecord
  | TopLevelStmt
  | Include
  | Other Text
  deriving (Show, Eq, Generic)

instance FromJSON CppDeclType where
  parseJSON = withText "CppDeclType" $ \t -> case t of
    "UsingDirective" -> pure UsingDirective
    "Var" -> pure Var
    "Function" -> pure Function
    "CXXRecord" -> pure CXXRecord
    "TopLevelStmt" -> pure TopLevelStmt
    "Include" -> pure Include
    other -> pure (Other other)

data CppDeclaration = CppDeclaration {
  declType :: CppDeclType
  , startLine :: Maybe Int
  , startCh :: Maybe Int
  , endLine :: Maybe Int
  , endCh :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON CppDeclaration where
  parseJSON = withObject "CppDeclaration" $ \o -> CppDeclaration
    <$> o .: "type"
    <*> o .: "start_line"
    <*> o .: "start_ch"
    <*> o .: "end_line"
    <*> o .: "end_ch"

parseCppDeclarations :: Text -> Either String [CppDeclaration]
parseCppDeclarations jsonText = eitherDecode (BL.fromStrict $ T.encodeUtf8 jsonText)

isImportLike :: CppDeclaration -> Bool
isImportLike (CppDeclaration {declType = UsingDirective}) = True
isImportLike (CppDeclaration {declType = Include}) = True
isImportLike _ = False

isExecutableStatement :: CppDeclaration -> Bool
isExecutableStatement (CppDeclaration {declType = TopLevelStmt}) = True
isExecutableStatement _ = False
