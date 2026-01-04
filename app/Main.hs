{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A hiding (Options)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message hiding (LookupFunc, parseClientMessage, parseServerMessage)
import Language.LSP.Protocol.Types
import Options.Applicative
import System.Exit
import System.IO
import System.Posix.Signals
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Temporary (withSystemTempDirectory)

import Transform.ClientNot
import Transform.ClientReq
import Transform.ClientRsp
import Transform.ServerNot
import Transform.ServerReq
import Transform.ServerRsp
import Transform.Util

import Parsing
import Process
import RequestMap
import Streams


data Options = Options {
  optWrappedLanguageServer :: Maybe FilePath
  , optWrappedArgs :: Maybe Text
  , optShadowDirTemplate :: Maybe FilePath
  , optDidSaveDebouncePeriodMs :: Int
  , optLogLevel :: Maybe Text
  }

options :: Parser Options
options = Options
  <$> optional (strOption (long "wrapped-server" <> help "Wrapped cpp-analyzer binary"))
  <*> optional (strOption (long "wrapped-args" <> help "Extra arguments to cpp-analyzer"))
  <*> optional (strOption (long "shadow-dir-template" <> help "Template for making a shadow project directory"))
  <*> option auto (long "did-save-period-ms" <> showDefault <> help "Debounce period for sending textDocument/didSave notifications after changes" <> value 1000 <> metavar "INT")
  <*> optional (strOption (long "log-level" <> help "Log level (debug, info, warn, error)"))

fullOpts :: ParserInfo Options
fullOpts = info (options <**> helper) (
  fullDesc <> progDesc "Run a wrapped cpp-analyzer with notebook support"
  )

main :: IO ()
main = do
  Options {..} <- execParser fullOpts

  wrappedLanguageServerPath <- (pure optWrappedLanguageServer <|> findExecutable "cpp-analyzer-wrapper") >>= \case
    Nothing -> throwIO $ userError [i|Couldn't find cpp-analyzer binary.|]
    Just x -> return x

  (Just wrappedIn, Just wrappedOut, Just wrappedErr, p) <- createProcess (
    (proc wrappedLanguageServerPath (maybe [] (fmap T.unpack . T.words) optWrappedArgs)) {
        close_fds = True
        , create_group = True
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        })

  hSetBuffering stdin NoBuffering -- TODO: LineBuffering here and below?
  hSetEncoding  stdin utf8

  hSetBuffering wrappedOut NoBuffering
  hSetEncoding  wrappedOut utf8

  hSetBuffering stdout LineBuffering
  hSetEncoding  stdout utf8

  hSetBuffering stderr LineBuffering
  hSetEncoding  stderr utf8

  clientReqMap <- newMVar newClientRequestMap
  serverReqMap <- newMVar newServerRequestMap

  -- TODO: switch to using pickFromIxMap or some other way to remove old entries

  logLevel <- case optLogLevel of
    Nothing -> return LevelInfo
    Just "debug" -> return LevelDebug
    Just "info" -> return LevelInfo
    Just "warn" -> return LevelWarn
    Just "error" -> return LevelError
    Just x -> throwIO $ userError [i|Unexpected log level: '#{x}' (must be one of debug, info, warn, error)|]

  let logFilterFn :: LogSource -> LogLevel -> Bool
      logFilterFn _src level = level >= logLevel

  let cleanup :: String -> IO ()
      cleanup signal = runStderrLoggingT $ do
        logInfoN [i|Got signal: #{signal}|]
        gracefullyStopProcess p 15_000_000

  void $ liftIO $ installHandler sigINT (Catch (cleanup "sigINT")) Nothing
  void $ liftIO $ installHandler sigTERM (Catch (cleanup "sigTERM")) Nothing

  stdoutLock <- newMVar ()

  let sendToStdout :: (MonadUnliftIO m, ToJSON a) => a -> m ()
      sendToStdout x = do
        withMVar stdoutLock $ \_ -> do
          liftIO $ writeToHandle stdout $ A.encode x

  let logFn :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      logFn _loc _src level msg = sendToStdout $ TNotificationMessage "2.0" SMethod_WindowLogMessage $ LogMessageParams {
        _type_ = levelToType level
        , _message = T.decodeUtf8 $ fromLogStr msg
        }

  withMaybeShadowDir optShadowDirTemplate $ \maybeShadowDir -> do
    transformerState <- newTransformerState maybeShadowDir optDidSaveDebouncePeriodMs

    flip runLoggingT logFn $ filterLogger logFilterFn $ flip runReaderT transformerState $
      withAsync (readWrappedOut clientReqMap serverReqMap wrappedOut sendToStdout) $ \_wrappedOutAsync ->
        withAsync (readWrappedErr wrappedErr) $ \_wrappedErrAsync ->
          withAsync (forever $ handleStdin wrappedIn clientReqMap serverReqMap) $ \_stdinAsync -> do
            waitForProcess p >>= \case
              ExitFailure n -> logErrorN [i|cpp-analyzer subprocess exited with code #{n}|]
              ExitSuccess -> logInfoN [i|cpp-analyzer subprocess exited successfully|]


withMaybeShadowDir :: MonadUnliftIO m => Maybe FilePath -> (FilePath -> m a) -> m a
withMaybeShadowDir maybeTemplate cb = withSystemTempDirectory "cpp-analyzer-shadow-home" $ \dir -> do
  case maybeTemplate of
    Just template -> do
      void $ readCreateProcess ((proc "cp" ["-r", "./.", dir]) { cwd = Just template }) ""
      void $ readCreateProcess (proc "chmod" ["-R", "u+w", dir]) ""
    Nothing -> return ()
  cb dir

handleStdin :: forall m. (
  MonadLoggerIO m, MonadReader TransformerState m, MonadUnliftIO m, MonadFail m
  ) => Handle -> MVar ClientRequestMap -> MVar ServerRequestMap -> m ()
handleStdin wrappedIn clientReqMap serverReqMap = do
  (A.eitherDecode <$> liftIO (parseStream stdin)) >>= \case
    Left err -> logErr [i|Couldn't decode incoming message: #{err}|]
    Right (x :: A.Value) -> do
      m <- readMVar serverReqMap
      case A.parseEither (parseClientMessage (lookupServerId m)) x of
        Left err -> do
          logErr [i|Couldn't decode incoming message: #{err}|]
          liftIO $ writeToHandle wrappedIn (A.encode x)
        Right (ClientToServerRsp meth msg) -> do
          transformClientRsp meth msg >>= liftIO . writeToHandle wrappedIn . A.encode
        Right (ClientToServerReq meth msg) -> do
          let msgId = msg ^. Lens.id
          modifyMVar_ clientReqMap $ \m -> case updateClientRequestMap m msgId (SMethodAndParams meth (msg ^. Lens.params)) of
            Just m' -> return m'
            Nothing -> return m
          transformClientReq meth msg >>= liftIO . writeToHandle wrappedIn . A.encode
        Right (ClientToServerNot meth msg) ->
          transformClientNot sendExtraNotification meth msg >>= (liftIO . writeToHandle wrappedIn . A.encode)
  where
    sendExtraNotification :: SendExtraNotificationFn m
    sendExtraNotification msg = do
      logDebugN [i|Sending extra notification: #{A.encode msg}|]
      liftIO $ writeToHandle wrappedIn $ A.encode msg

readWrappedOut :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadReader TransformerState m, MonadFail m
  ) => MVar ClientRequestMap -> MVar ServerRequestMap -> Handle -> (forall a. ToJSON a => a -> m ()) -> m b
readWrappedOut clientReqMap serverReqMap wrappedOut sendToStdout = forever $ do
  (A.eitherDecode <$> liftIO (parseStream wrappedOut)) >>= \case
    Left err -> logErr [i|Couldn't decode wrapped output: #{err}|]
    Right (x :: A.Value) -> do
      m <- readMVar clientReqMap
      case A.parseEither (parseServerMessage (lookupClientId m)) x of
        Left err -> do
          logErr [i|Couldn't decode server message: #{A.encode x} (#{err})|]
          sendToStdout x
        Right (ServerToClientNot meth msg) ->
          transformServerNot meth msg >>= sendToStdout
        Right (ServerToClientReq meth msg) -> do
          let msgId = msg ^. Lens.id
          modifyMVar_ serverReqMap $ \m -> case updateServerRequestMap m msgId (SMethodAndParams meth (msg ^. Lens.params)) of
            Just m' -> return m'
            Nothing -> return m
          sendToStdout (transformServerReq meth msg)
        Right (ServerToClientRsp meth initialParams msg) ->
          transformServerRsp meth initialParams msg >>= sendToStdout

readWrappedErr :: MonadLoggerIO m => Handle -> m ()
readWrappedErr wrappedErr = forever $ do
  line <- liftIO (hGetLine wrappedErr)
  logErrorN [i|(wrapped stderr) #{line}|]

lookupServerId :: ServerRequestMap -> LookupFunc 'ServerToClient
lookupServerId serverReqMap sid = do
  case lookupServerRequestMap serverReqMap sid of
    Nothing -> Nothing
    Just (SMethodAndParams meth initialParams) -> Just (meth, initialParams)

lookupClientId :: ClientRequestMap -> LookupFunc 'ClientToServer
lookupClientId clientReqMap sid = do
  case lookupClientRequestMap clientReqMap sid of
    Nothing -> Nothing
    Just (SMethodAndParams meth initialParams) -> Just (meth, initialParams)

logErr :: MonadLoggerIO m => Text -> m ()
logErr = logInfoN

writeToHandle :: Handle -> BL8.ByteString -> IO ()
writeToHandle h bytes = do
  BL8.hPutStr h [i|Content-Length: #{BL.length bytes}\r\n\r\n#{bytes}|]
  hFlush h

levelToType :: LogLevel -> MessageType
levelToType LevelDebug = MessageType_Log
levelToType LevelInfo = MessageType_Info
levelToType LevelWarn = MessageType_Warning
levelToType LevelError = MessageType_Error
levelToType (LevelOther _typ) = MessageType_Info
