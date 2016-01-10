import System.Process
import System.IO
import Control.Monad
import Control.Arrow
import Control.Concurrent
import System.Exit



data LineBufferedHandle = LineBufferedHandle { handle :: Handle
                                             , buffer :: String
                                             }

lineBuffered :: Handle -> LineBufferedHandle
lineBuffered h = LineBufferedHandle { handle = h
                                    , buffer = ""
                                    }

hGetCharNonBlocking :: Handle -> IO (Maybe Char)
hGetCharNonBlocking handle = do
  ready <- hReady handle
  if ready then (liftM return) $ hGetChar handle else return Nothing
      

readLineNonBlocking :: LineBufferedHandle -> IO (LineBufferedHandle, Maybe String)
readLineNonBlocking lbh = let
  h = handle lbh
  buf = buffer lbh
  in do
    maybeChar <- hGetCharNonBlocking h
    case maybeChar of
      Nothing             -> return (lbh, Nothing)
      Just c  | c == '\n' -> return (LineBufferedHandle {handle = h, buffer = []}, Just $ reverse buf)
      Just c              -> readLineNonBlocking $ LineBufferedHandle { handle = h
                                                                      , buffer = c : buf
                                                                      }

readAvailableLines :: LineBufferedHandle -> IO (LineBufferedHandle, [String])
readAvailableLines lbh = (loop lbh []) >>= (return . (id *** reverse))
  where
    loop :: LineBufferedHandle -> [String] -> IO (LineBufferedHandle, [String])
    loop lbh lines = do
      (lbh, maybeLine) <- readLineNonBlocking lbh
      case maybeLine of
        Nothing   -> return (lbh, lines)
        Just line -> loop lbh (line : lines)


launchInotify :: String -> IO Handle
launchInotify watchPath = do
  let procCmd = (proc "/usr/bin/inotifywait" ["-e", "close_write,move,delete", "-r", "-m", watchPath]) { std_out = CreatePipe }
  (_, Just outHandle, _, _) <- createProcess procCmd
  return outHandle


filePathFromInotifyLine :: String -> FilePath
filePathFromInotifyLine line = dirpath ++ filename
  where
    ws = words line
    dirpath = head ws
    eventstr = head $ tail ws
    filename = join $ tail $ tail ws


onFileChange :: FilePath -> ([FilePath] -> IO ()) -> IO () 
onFileChange path action = do
  inotifyOut <- (launchInotify path) >>= (return . lineBuffered)

  let
    loop :: LineBufferedHandle -> IO ()
    loop lbh = do
      (lbh, lines) <- readAvailableLines lbh
      if null lines 
        then hWaitForInput (handle lbh) (-1) >> return ()
        else action (map filePathFromInotifyLine lines)
      loop lbh

  loop inotifyOut


isGitRepo :: IO Bool
isGitRepo = do
  (exitCode, _, _) <- readProcessWithExitCode "/usr/bin/git" ["status"] ""
  case exitCode of
    ExitSuccess     -> return True
    ExitFailure 128 -> return False
    otherwise       -> error "git status error"


isIgnored :: FilePath -> IO Bool
isIgnored path = do
  (exitCode, _, _) <- readProcessWithExitCode "/usr/bin/git" ["check-ignore", path] ""
  case exitCode of
    ExitSuccess   -> return True
    ExitFailure 1 -> return False
    otherwise     -> error "git check-ignore error"


printLines :: LineBufferedHandle -> IO ()
printLines handle = do
  (handle, maybeLine) <- readLineNonBlocking handle
  case maybeLine of
    Nothing   -> (threadDelay $ 1000000 * 5) >> printLines handle
    Just line -> putStrLn line >> printLines handle


rebuildNecessary :: [FilePath] -> IO Bool
rebuildNecessary changedFilePaths = do
  ignoredFlags <- sequence $ map isIgnored changedFilePaths
  return $ not $ and ignoredFlags
  
-- rebuildIfNecessary :: [FilePath] -> IO () -> IO ()
-- rebuildIfNecessary paths rebuild = do
--   ignoredFlags <- sequence $ map isIgnored paths
--   if and ignoredFlags
--     then rebuild
--     else return ()

rebuild :: [FilePath] -> IO ()
rebuild changedFilePaths = do
  necessary <- rebuildNecessary changedFilePaths
  if necessary
    then (readCreateProcess (shell "ghc autorebuild.hs") "") >>= putStrLn
    else return ()


main :: IO ()
main = onFileChange "." rebuild
                           
  -- threadDelay $ 1000000 * 5
  -- inotifyOut <- (launchInotify ".") >>= (return . lineBuffered)
  -- print "123"
  -- printLines inotifyOut

