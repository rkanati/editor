
module Main where

import qualified Editor.Buffer as B
import           Editor.Buffer (Buffer)

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr

import qualified System.Posix as Px
import System.Posix.IOCtl
import System.IO

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (Exception, throwIO)
--import Control.Monad.Except
import Control.Monad.RWS.Strict

import           Data.Char
import qualified Data.Text         as T
import qualified Data.Text.Foreign as TF
import           Data.Text (Text)
import           Data.Typeable
import           Data.Word
import           Data.Traversable
import           Data.Foldable

fI = fromIntegral

showT = T.pack . show

data Dims = Dims !Int !Int

render :: Dims -> Buffer -> Text
render (Dims w h) buf = T.concat rows where
  rows = fmap (T.take w . (<> "\r\n")) lines
  lines = B.window buf 0 h

data TTY
  = TTY
    { fdTTY :: Px.Fd
    , hTTY  :: Handle
    }

type MonadTTY m = (MonadReader TTY m, MonadIO m)

io = liftIO
io_ = void . io

putTTY :: (MonadTTY m) => Text -> m ()
putTTY !text = do
  tty <- ask
  io_ $ TF.withCStringLen text $ \(ptr, n) ->
    hPutBuf (hTTY tty) ptr n

csi :: (MonadTTY m) => Text -> m ()
csi s = putTTY ("\ESC[" <> s)

clearScreen :: (MonadTTY m) => m ()
clearScreen = csi "2J"

cursorOrigin :: (MonadTTY m) => m ()
cursorOrigin = csi "1;1H"

setCursor :: (MonadTTY m) => (Int,Int) -> m ()
setCursor (c,r) = csi $ showT r <> ";" <> showT c <> "H"

data TTYIOCTL = TIOCGWINSZ

data WinSize
  = WinSize
  { row :: Word16
  , col :: Word16
  , xpx :: Word16
  , ypx :: Word16
  }

instance Storable WinSize where
  sizeOf _ = 4 * sizeOf (0 :: Word16)
  alignment _ = alignment (0 :: Word16)
  peek ptr
    =   WinSize
    <$> peekElemOff ptr' 0
    <*> peekElemOff ptr' 1
    <*> peekElemOff ptr' 2
    <*> peekElemOff ptr' 3
    where ptr' = castPtr ptr
  poke ptr ws = do
    pokeElemOff ptr' 0 (row ws)
    pokeElemOff ptr' 1 (col ws)
    pokeElemOff ptr' 2 (xpx ws)
    pokeElemOff ptr' 3 (ypx ws)
    where ptr' = castPtr ptr

instance IOControl TTYIOCTL WinSize where
  ioctlReq TIOCGWINSZ = 0x5413

getSize :: (MonadTTY m) => m (Int, Int)
getSize = do
  tty <- ask
  ws <- io $ ioctl' (fdTTY tty) TIOCGWINSZ
  pure (fI $ col ws, fI $ row ws)

many_ = void . many

data Event
  = Exit
  | Winch
  | Input

openTTY :: IO TTY
openTTY = do
  -- need both fd and handle for stupid reasons
  fd <- Px.openFd "/dev/tty" Px.ReadWrite Nothing Px.defaultFileFlags
  h <- Px.fdToHandle fd
  hSetBuffering h NoBuffering

  -- go raw
  attrs <- Px.getTerminalAttributes fd
  let attrs' = foldl' Px.withoutMode attrs
        [ Px.IgnoreBreak, Px.InterruptOnBreak
        , Px.CheckParity, Px.EnableParity
        , Px.StripHighBit
        , Px.MapLFtoCR, Px.MapCRtoLF, Px.IgnoreCR
        , Px.StartStopOutput, Px.ProcessOutput
        , Px.EnableEcho, Px.EchoLF, Px.ProcessInput
        , Px.ExtendedFunctions
        ]
  Px.setTerminalAttributes fd attrs' Px.Immediately

  pure (TTY fd h)

-- subsystems
data Env
  = Env
    { tty        :: TTY
    , events     :: Chan Event
    , pollAction :: (forall m. MonadIO m => m ())
    }

setup :: IO Env
setup = do
  tty <- openTTY
  events <- newChan

  -- handle SIGWINCH
  Px.installHandler 28 (Px.Catch $ writeChan events Winch) Nothing

  -- handle terminal input
  let pollAction :: forall m. MonadIO m => m ()
      pollAction = io_.void.forkIO $ do
        hWaitForInput (hTTY tty) (-1)
        writeChan events Input

  pollAction

  pure $ Env tty events pollAction

-- state
data Mode
  = Normal
  | Insert
  deriving (Show)

data St
  = St
    { buffer  :: Buffer
    , command :: Text
    , mode    :: Mode
    }

state0
  = St
    { buffer  = B.load $ T.unlines $ take 100 $ fmap ((<>" awoijf!").showT) [1..]
    , command = ""
    , mode    = Normal
    }

-- application
type App a = RWST Env () St IO a

data AppError
  = AppExit
  deriving (Show, Typeable)

instance Exception AppError

modBuffer :: (Buffer -> Buffer) -> App ()
modBuffer f = modify $ \s -> s { buffer = f (buffer s) }

pollInput :: App ()
pollInput = asks pollAction >>= id

clearCommand :: App ()
clearCommand = modify $ \s -> s { command = "" }

tryCommand :: App ()
tryCommand = do
  cmd <- gets command
  clearCommand
  case cmd of
    "gg" -> modBuffer B.moveToTop
    "G"  -> modBuffer B.moveToBottom
    "j" -> modBuffer (B.moveDown 1)
    "k" -> modBuffer (B.moveUp   1)
    "i" -> modify (\s -> s { mode = Insert })
    "q" -> io $ throwIO AppExit
    _   -> modify $ \s -> s { command = cmd } -- put it back

handleChar :: Char -> App ()
handleChar c = gets mode >>= \case
  Normal -> case c of
    '\ESC' -> modify $ \s -> s { command = "" }
    '\r' -> do
      tryCommand
      clearCommand
    c | c `elem` ['\BS', '\DEL'] ->
          modify $ \s -> s { command = T.dropEnd 1 (command s) }
      | isPrint c -> do
          modify $ \s -> s { command = command s `T.snoc` c }
          tryCommand
      | otherwise -> pure ()
  Insert -> case c of
    '\ESC' -> modify $ \s -> s { mode = Normal }
    c      -> modBuffer (B.insert c)

handle :: Event -> App ()
handle = \case
  Input -> do
    h <- asks (hTTY.tty)

    -- process one character at a time
    many_ $ do
      guard =<< (io.hReady) h
      handleChar =<< (io.hGetChar) h

    -- register for more
    pollInput

  Exit -> io $ throwIO AppExit

  _ -> pure ()

liftTTY :: (forall m. MonadTTY m => m a) -> App a
liftTTY = withRWST (\env st -> (tty env, st))

draw :: App ()
draw = do
  st <- get
  liftTTY $ do
    clearScreen
    cursorOrigin
    (w,h) <- getSize
    putTTY $ render (Dims w (h-1)) (buffer st)
    setCursor (0,h)
    putTTY $ showT (mode st) <> " - " <> command st

run :: Env -> St -> App a -> IO ()
run e s a = void $ runRWST (many_ a) e s

main :: IO ()
main = do
  env <- setup
  run env state0 $ do
    draw
    wait >>= handle

  where
    wait :: App Event
    wait = io . readChan =<< asks events

