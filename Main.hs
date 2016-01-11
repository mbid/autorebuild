import System.Process
import System.IO
import System.Exit
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Options.Applicative




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


isGitIgnored :: FilePath -> IO Bool
isGitIgnored path = do
  (exitCode, _, _) <- readProcessWithExitCode "/usr/bin/git" ["check-ignore", path] ""
  case exitCode of
    ExitSuccess   -> return True
    ExitFailure 1 -> return False
    otherwise     -> error "git check-ignore error"


watchAndExecute :: FilePath -> (FilePath -> IO Bool) -> CreateProcess -> IO ()
watchAndExecute filePath ignorePredicate process = onFileChange filePath onChange
  where
    onChange :: [FilePath] -> IO ()
    onChange filePaths = do
      ignoreFlags <- sequence $ map ignorePredicate filePaths
      when (not $ and ignoreFlags) $ do
        (_, stdout, stderr) <- readCreateProcessWithExitCode process ""
        putStrLn stdout

data Options = Options
  { shellCommand :: String
  , directory :: FilePath 
  , ignoreGit :: Bool }
  deriving Show

optionsParser :: Parser Options
optionsParser = Options
            <$> strArgument (metavar "COMMAND")
            <*> strOption 
                ( long "dir"
               <> short 'd'
               <> metavar "DIRECTORY"
               <> help "Watch for changed files in DIRECTORY instead of './'"
               <> value "./" )
            <*> switch 
                ( long "no-git"
               <> help "don't ignore files ignored by git (if any)" )

optionsParserInfo = info (helper <*> optionsParser)
                    ( fullDesc 
                   <> progDesc "Execute shell command 'COMMAND' when file in DIRECTORY changes"
                   <> header "autorebuild - a utility for automatic rebuilds")

main :: IO ()
main = do
  opts <- execParser optionsParserInfo
  let 
    process = shell $ shellCommand opts
    dir = directory opts
    ignorePredicate = if (not $ ignoreGit opts) -- && (not $ isGitIgnored dir)
                        then \filePath -> isGitIgnored $ filePath
                        else \_ -> return False  
  watchAndExecute dir ignorePredicate process