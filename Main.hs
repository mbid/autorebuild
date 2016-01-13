import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Options.Applicative
import Data.List
import Data.IORef



data LineBufferedHandle = LineBufferedHandle { handle :: Handle
                                             , buffer :: String }

lineBuffered :: Handle -> LineBufferedHandle
lineBuffered h = LineBufferedHandle { handle = h
                                    , buffer = "" }

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
                                                                      , buffer = c : buf }

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
  let procCmd = (proc "/usr/bin/inotifywait" ["-e", "close_write,move,delete", "-r", "-m", watchPath])
                { std_out = CreatePipe 
                , std_err = CreatePipe } -- This could be a memory leak... NoStream would be the right way
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

gitInstalled :: IO Bool
gitInstalled = doesFileExist "/usr/bin/git"

isGitRepository :: FilePath -> IO Bool
isGitRepository filePath = do
  let process = (proc "/usr/bin/git" ["status"]) {cwd = Just filePath}
  (exitCode, _, _) <- readCreateProcessWithExitCode process ""
  case exitCode of
    ExitSuccess     -> return True
    ExitFailure 128 -> return False
    otherwise       -> error "git status error"


isGitIgnored :: FilePath -> IO Bool
isGitIgnored filePath = do
  -- quick-and-dirty hack to detect files in .git directory
  if ".git/" `isInfixOf` filePath
    then return True
    else do
      let process = (proc "/usr/bin/git" ["check-ignore", filePath])
      (exitCode, _, _) <- readCreateProcessWithExitCode process ""
      case exitCode of
        ExitSuccess   -> return True
        ExitFailure 1 -> return False
        otherwise     -> error "git check-ignore error"

mLazyAnd :: Monad m => [m Bool] -> m Bool
mLazyAnd []         = return True
mLazyAnd (mb : mbs) = do
  b <- mb
  if b
    then mLazyAnd mbs
    else return False

watchAndExecute :: FilePath -> (FilePath -> IO Bool) -> IO () -> IO ()
watchAndExecute filePath ignorePredicate action = onFileChange filePath onChange
  where
    onChange :: [FilePath] -> IO ()
    onChange filePaths = do
      ignoreFileChange <- mLazyAnd $ map ignorePredicate filePaths
      when (not ignoreFileChange) $ action

data Options = Options
  { shellCommand :: String
  , directory :: FilePath 
  , ignoreGit :: Bool }
  deriving Show

optionsParser :: Parser Options
optionsParser = Options
            <$> strArgument (metavar "COMMAND")
            <*> strOption 
                ( long "watch-dir"
               <> short 'd'
               <> metavar "WATCH_DIR"
               <> help "Watch for changed files in WATCH_DIR instead of './'"
               <> value "./" )
            <*> switch 
                ( long "no-git"
               <> help "don't ignore files ignored by git (if any)" )

optionsParserInfo = info (helper <*> optionsParser)
                    ( fullDesc 
                   <> progDesc "Execute shell command 'COMMAND' whenever a file below the current directory changes"
                   <> header "autorebuild - a utility for automatic rebuilds")


newtype LessProcess = LessProcess ProcessHandle

callInLess :: CreateProcess -> IO LessProcess
callInLess outputProc = do
  (Just lessStdIn, _, _, lessProcHandle) <- createProcess $ (proc "/usr/bin/less" []) {std_in = CreatePipe}
  (_, _, _, outputProcHandle) <- createProcess $ outputProc { std_out = UseHandle lessStdIn
                                                            , std_err = UseHandle lessStdIn }
  
  waitForProcess outputProcHandle

  return $ LessProcess lessProcHandle

stopLess :: LessProcess -> IO ()
stopLess (LessProcess lessProcHandle) = do
  terminateProcess lessProcHandle
  callProcess "/usr/bin/stty" []

main :: IO ()
main = do
  opts <- execParser optionsParserInfo
  let 
    process = shell $ shellCommand opts
    dir = directory opts
    gitPredicateConditions = [ return $ not $ ignoreGit opts
                             , gitInstalled
                             , isGitRepository dir ]
    trivialIgnorePredicate :: FilePath -> IO Bool
    trivialIgnorePredicate _ = return False

  useGit <- mLazyAnd gitPredicateConditions

  let ignorePredicate = if useGit
                          then isGitIgnored
                          else \_ -> return False

  refLessProc <- callInLess process >>= newIORef
  let rebuild = do
                  lessProc <- readIORef refLessProc 
                  stopLess lessProc
                  lessProc <- callInLess process
                  writeIORef refLessProc lessProc

  watchAndExecute dir ignorePredicate rebuild
