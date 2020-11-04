import Control.Concurrent
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Directory
import System.FilePath ((</>))
import System.IO
import System.IO.Temp
import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Array.Storable.Foreign (Array)
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . Stream.fromList

watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event.Event
watchPaths = Event.watchTrees

timeout :: IO String
timeout = threadDelay 5000000 >> return "Timeout"    

data Sync = Sync (MVar String)

fseventDir :: String
fseventDir = "fsevent_dir"

postEvent :: Sync -> ThreadId -> IO String
postEvent (Sync m) tid = do
    res <- takeMVar m
    killThread tid
    return res 

-------------------------------------------------------------------------------
-- Event lists to be matched with
-------------------------------------------------------------------------------
matchSingleDirs :: [String]
matchSingleDirs = 
    [ "dir1Single_1073742080_Dir"
    , "dir1Single_1073741856_Dir"
    , "dir1Single_1073741825_Dir"
    , "dir1Single_1073741840_Dir"
    ]

matchNestedDirs :: [String]
matchNestedDirs = 
    [ "dir1_1073742080_Dir"
    , "dir1_1073741856_Dir"
    , "dir1_1073741825_Dir"
    , "dir1_1073741840_Dir"
    ]    

matchCreateFiles :: [String]
matchCreateFiles = 
    [ "FileCreated.txt_256"
    , "FileCreated.txt_32"
    , "FileCreated.txt_2"   
    ]

-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------
eventWatcher ::  Int -> FilePath -> Sync -> Sync -> [String] -> IO ()
eventWatcher n rootPath (Sync m1) (Sync m2) matchList = do
    let args = [rootPath]
    paths <- mapM toUtf8 args    
    putStrLn ("Watch started !!!!!!!!!!!!!!!!!!!! " ++ rootPath)
    events <- Stream.parse (PR.take n FL.toList) 
        $ Stream.before (putMVar m1 (""))
        $ watchPaths (NonEmpty.fromList paths)
    let eventStr =  map Event.showEventShort events
    putStrLn $ show (eventStr)    
    if (eventStr == matchList)
    then 
        putMVar m2 ("PASS")
    else
        putMVar m2 ("Mismatch")  

-------------------------------------------------------------------------------
-- FS Event Generators
-------------------------------------------------------------------------------
fsOpsCreateFileInRootDir :: [String] -> Sync -> Sync -> IO String
fsOpsCreateFileInRootDir matches sync1@(Sync m1) sync2 = do    
    withSystemTempDirectory fseventDir $ \fp -> do
        let tpath = (fp </> "FileCreated.txt")        
        tid <- forkIO $ eventWatcher 3 fp sync1 sync2 matches
        _ <- takeMVar m1
        putStrLn ("create a File  on " ++ fp)
        writeFile tpath "Test Data"
        postEvent sync2 tid
 
fsOpsCreateSingleDir :: [String] -> Sync -> Sync -> IO String
fsOpsCreateSingleDir matches sync1@(Sync m1) sync2 = do    
    withSystemTempDirectory fseventDir $ \fp -> do
        tid <- forkIO $ eventWatcher 4 fp sync1 sync2 matches
        _ <- takeMVar m1
        putStrLn ("CreateDirectory Single!!!!!!!!!!!!! on " ++ fp)
        createDirectoryIfMissing True (fp </> "dir1Single")
        postEvent sync2 tid

fsOpsCreateNestedDir :: [String] -> Sync -> Sync -> IO String
fsOpsCreateNestedDir matches sync1@(Sync m1) sync2 = do    
    withSystemTempDirectory fseventDir $ \fp -> do
        tid <- forkIO $ eventWatcher 4 fp sync1 sync2 matches
        _ <- takeMVar m1
        putStrLn ("Create Nested Directory !!!!!!!!!!!!! on " ++ fp)
        createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
        postEvent sync2 tid

checker :: IO String -> IO (Maybe [Char])
checker f = Stream.head 
    $ Stream.yieldM f `Stream.parallelFst` Stream.yieldM timeout

checkerTemplate :: (Sync -> Sync -> IO String) -> IO String
checkerTemplate fOps = do
    hSetBuffering stdout NoBuffering
    pre <- newEmptyMVar
    post <- newEmptyMVar    
    res <- checker (fOps (Sync pre) (Sync post))
    return $ fromJust res   

-------------------------------------------------------------------------------
-- Test Drivers
-------------------------------------------------------------------------------
driverCreateSingleDir :: IO String
driverCreateSingleDir = checkerTemplate 
    $ fsOpsCreateSingleDir matchSingleDirs

driverCreateNestedDir :: IO String
driverCreateNestedDir = checkerTemplate 
    $ fsOpsCreateNestedDir matchNestedDirs

driverCreateFileInRootDir :: IO String
driverCreateFileInRootDir = checkerTemplate 
    $ fsOpsCreateFileInRootDir matchCreateFiles        

-------------------------------------------------------------------------------
-- Test Cases
-------------------------------------------------------------------------------
testCreateSingleDir :: Expectation
testCreateSingleDir = driverCreateSingleDir `shouldReturn` "PASS"

testCreateNestedDir :: Expectation
testCreateNestedDir = driverCreateNestedDir `shouldReturn` "PASS"

testCreateFileInRootDir :: Expectation
testCreateFileInRootDir = driverCreateFileInRootDir `shouldReturn` "PASS"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
    prop "Create a single directory" testCreateSingleDir 
    prop "Create a nested directory" testCreateNestedDir 
    prop "Create a file in root Dir" testCreateFileInRootDir