{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}

module TestLib.Generators (
  arbitrarySingleLineChange
  , arbitraryChange
  ) where

import Data.Diff.Myers
import qualified Data.Diff.Types as DT
import Data.Function
import qualified Data.List as L
import Data.Row.Records
import Data.Text as T
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text ()
import TestLib.Util


arbitrarySingleLineChange :: Doc -> Gen TextDocumentContentChangeEvent
arbitrarySingleLineChange (docToList -> docLines) = do
  lineNo <- chooseInt (0, fromIntegral $ L.length docLines - 1)
  let line = docLines L.!! (fromIntegral lineNo)

  let lineLen = T.length line
  pos1 <- chooseInt (0, max 0 (fromIntegral (lineLen - 1)))
  pos2 <- chooseInt (pos1, (max pos1 (fromIntegral (lineLen - 1))))

  toInsert :: String <- arbitrary

  pure $ TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial (Range (p lineNo pos1) (p lineNo pos2)) Nothing (T.pack toInsert)

arbitraryChange :: Doc -> Gen TextDocumentContentChangeEvent
arbitraryChange = undefined

arbitraryChanges :: Doc -> Gen [TextDocumentContentChangeEvent]
arbitraryChanges doc = do
  (_, doc') <- arbitraryChangesSized doc
  let events = diffTextsToChangeEventsConsolidate (Rope.toText doc) (Rope.toText doc')
  return $ fmap repackChangeEvent events
  where
    repackChangeEvent (DT.ChangeEvent range text) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial (repackRange range) Nothing text
    repackRange (DT.Range (DT.Position l1 c1) (DT.Position l2 c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

-- * Newtypes to make it easier to generate certain combinations

newtype InsertOrDelete = InsertOrDelete (Rope, Rope)
  deriving (Show, Eq)
instance Arbitrary InsertOrDelete where
  arbitrary = InsertOrDelete <$> oneof [arbitraryInsert, arbitraryDelete]

newtype DocInsertOrDelete = DocInsertOrDelete (Rope, Rope)
  deriving (Show, Eq)
instance Arbitrary DocInsertOrDelete where
  arbitrary = DocInsertOrDelete <$> oneof [arbitraryDocInsert, arbitraryDocDelete]

newtype MultiInsertOrDelete = MultiInsertOrDelete (Rope, Rope)
  deriving (Show, Eq)
instance Arbitrary MultiInsertOrDelete where
  arbitrary = arbitrary >>= (MultiInsertOrDelete <$>) . arbitraryChangesSized

newtype DocMultiInsertOrDelete = DocMultiInsertOrDelete (Rope, Rope)
  deriving (Show, Eq)
instance Arbitrary DocMultiInsertOrDelete where
  arbitrary = arbitrary >>= (DocMultiInsertOrDelete <$>) . arbitraryChangesSized

-- * Apply a series of changes

arbitraryChangesSized :: Rope -> Gen (Rope, Rope)
arbitraryChangesSized initial = sized $ \n -> flip fix (n, initial) $ \loop -> \case
  (0, x) -> return (initial, x)
  (j, cur) -> do
    -- Prefer inserts to deletes at a 3:1 ratio
    next <- oneof [arbitraryInsertOn cur, arbitraryInsertOn cur, arbitraryInsertOn cur, arbitraryDeleteOn cur]
    loop (j - 1, next)

-- -- * Inserts and deletes

-- | Arbitrarily insert up to size parameter characters
arbitraryDocInsert :: Gen (Rope, Rope)
arbitraryDocInsert = arbitraryDoc >>= (\initial -> (initial, ) <$> arbitraryInsertOn initial)

arbitraryInsert :: Gen (Rope, Rope)
arbitraryInsert = arbitrary >>= (\initial -> (initial, ) <$> arbitraryInsertOn initial)

arbitraryInsertOn :: Rope -> Gen Rope
arbitraryInsertOn initial = do
  toInsert <- arbitrary

  pos <- chooseBoundedIntegral (0, fromIntegral $ Rope.length initial)
  let (x, y) = Rope.splitAt pos initial

  return (x <> toInsert <> y)


arbitraryDelete :: Gen (Rope, Rope)
arbitraryDelete = arbitrary >>= (\initial -> (initial, ) <$> arbitraryDeleteOn initial)

arbitraryDocDelete :: Gen (Rope, Rope)
arbitraryDocDelete = arbitraryDoc >>= (\initial -> (initial, ) <$> arbitraryDeleteOn initial)

-- | Arbitrarily delete up to size parameter characters
arbitraryDeleteOn :: Rope -> Gen Rope
arbitraryDeleteOn initial = do
  size <- getSize
  amountToDelete <- chooseBoundedIntegral (1, fromIntegral size)

  pos1 <- chooseBoundedIntegral (0, max 0 (Rope.length initial - 1))
  pos2 <- chooseBoundedIntegral (pos1, min (pos1 + amountToDelete) (Rope.length initial))

  let (x, y) = Rope.splitAt pos1 initial
  let (_, z) = Rope.splitAt (pos2 - pos1) y

  return (x <> z)

-- * Docs

instance Arbitrary Rope where
  arbitrary = Rope.fromText <$> arbitrary

arbitraryLine :: Gen Text
arbitraryLine = oneof [pure "", arbitrary]

arbitraryDoc :: Gen Rope
arbitraryDoc = listToDoc <$> listOf arbitraryLine

arbitraryAlphanumericString :: Gen Rope
arbitraryAlphanumericString = (Rope.fromText . T.pack) <$> listOf alphaNumericChar

alphaNumericChar :: Gen Char
alphaNumericChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '])
