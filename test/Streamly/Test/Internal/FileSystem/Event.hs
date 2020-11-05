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
import qualified Streamly.Internal.Data.Stream.IsStream as S
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
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . S.fromList

watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event.Event
watchPaths = Event.watchTrees

timeout :: IO String
timeout = threadDelay 5000000 >> return "Timeout"    

data Sync = Sync (MVar ())

fseventDir :: String
fseventDir = "fsevent_dir"

-------------------------------------------------------------------------------
-- Event lists to be matched with
-------------------------------------------------------------------------------
#if defined(CABAL_OS_WINDOWS)
singleDirEvents :: [String]
singleDirEvents =     
    [ "dir1Single_1" ]

nestedDirEvents :: [String]
nestedDirEvents = 
    [ "dir1_1"
    , "dir1\\dir2_1"
    , "dir1\\dir2\\dir3_1"
    ]

createFileEvents :: [String]
createFileEvents = 
    [ "FileCreated.txt_1"
    , "FileCreated.txt_3"
    , "FileCreated.txt_3"
    ]
#else 
singleDirEvents :: [String]
singleDirEvents = 
    [ "dir1Single_1073742080_Dir"
    , "dir1Single_1073741856_Dir"
    , "dir1Single_1073741825_Dir"
    , "dir1Single_1073741840_Dir"
    ]

nestedDirEvents :: [String]
nestedDirEvents = 
    [ "dir1_1073742080_Dir"
    , "dir1_1073741856_Dir"
    , "dir1_1073741825_Dir"
    , "dir1_1073741840_Dir"
    ]  
   
createFileEvents :: [String]
createFileEvents = 
    [ "FileCreated.txt_256"
    , "FileCreated.txt_32"
    , "FileCreated.txt_2"   
    ]
#endif

-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------
checkEvents :: FilePath -> Sync -> [String] -> IO String
checkEvents rootPath (Sync m) matchList = do    
    let args = [rootPath]
        eventCount = length matchList
    paths <- mapM toUtf8 args    
    putStrLn ("Watch started !!!! on Path " ++ rootPath)
    events <- S.parse (PR.take eventCount FL.toList) 
        $ S.before (putMVar m ())
        $ watchPaths (NonEmpty.fromList paths)
    let eventStr =  map Event.showEventShort events
    putStrLn $ show (eventStr)    
    if (eventStr == matchList)
    then 
        return "PASS"
    else
        return "Mismatch"         

-------------------------------------------------------------------------------
-- FS Event Generators
------------------------------------------------------------------------------- 
fsOpsCreateSingleDir :: FilePath -> Sync -> IO ()
fsOpsCreateSingleDir fp (Sync m) = do
    takeMVar m
    putStrLn ("Create Single Directory !!!!!!! on " ++ fp)
    createDirectoryIfMissing True (fp </> "dir1Single")       

fsOpsCreateNestedDir :: FilePath -> Sync -> IO ()
fsOpsCreateNestedDir fp (Sync m) = do    
    takeMVar m
    putStrLn ("Create Nested Directory !!!!!!!!!!!!! on " ++ fp)
    createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")

fsOpsCreateFileInRootDir :: FilePath -> Sync -> IO ()
fsOpsCreateFileInRootDir fp (Sync m) = do                
    takeMVar m
    let tpath = (fp </> "FileCreated.txt") 
    putStrLn ("create a File  on " ++ fp)
    writeFile tpath "Test Data"       

checker :: S.IsStream t =>
                FilePath -> Sync -> [String] -> t IO String
checker rootPath synch matchList = 
    S.yieldM (checkEvents rootPath synch matchList) 
    `S.parallelFst` 
    S.yieldM timeout

driverInit :: IO Sync
driverInit = do
    hSetBuffering stdout NoBuffering
    pre <- newEmptyMVar
    return (Sync pre)

-------------------------------------------------------------------------------
-- Test Drivers
-------------------------------------------------------------------------------
driverCreateSingleDir :: IO String
driverCreateSingleDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        res <- S.head 
            $ (checker fp sync singleDirEvents) 
            `S.parallelFst` 
            S.yieldM  -- ^ this message should follow checker
            (fsOpsCreateSingleDir fp sync 
                >> threadDelay 10000000 
                >> return "fOps Done")
        return $ fromJust res

driverCreateNestedDir :: IO String
driverCreateNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        res <- S.head 
            $ (checker fp sync nestedDirEvents) 
            `S.parallelFst` 
            S.yieldM 
            (fsOpsCreateNestedDir fp sync 
                >> threadDelay 10000000 
                >> return "fOps Done")
        return $ fromJust res

driverCreateFileInRootDir :: IO String
driverCreateFileInRootDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        res <- S.head 
            $ (checker fp sync createFileEvents) 
            `S.parallelFst` 
            S.yieldM 
            (fsOpsCreateFileInRootDir fp sync 
                >> threadDelay 10000000 
                >> return "fOps Done")
        return $ fromJust res       

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