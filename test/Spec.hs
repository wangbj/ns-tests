{-# LANGUAGE LambdaCase #-}

import System.Posix.Process
import System.Posix.Types
import System.Linux.Namespaces
import Test.Hspec
import Control.Exception
import System.IO.Error

runChild :: IO ()
runChild = hspec $ do
  describe "running child" $ do
    it "first process pid must be 1" $ do
      getProcessID `shouldReturn` 1

exitedNormally :: ProcessID -> IO ()
exitedNormally pid = getProcessStatus True True pid >>= \case
    Nothing -> throwIO (userError "getProcessStatus")
    Just (Exited _) -> return ()
    Just (Terminated sig _) -> throwIO (userError ("child terminated by signal: " ++ show sig))
    Just (Stopped sig)      -> throwIO (userError ("child stopped by signal: " ++ show sig))

getPidNs = hspec $ do
  describe "create new PID name space" $ do
    it "child must exit normally" $ do
      unshare [PID, User]
      pid <- forkProcess runChild
      exitedNormally pid `shouldReturn` ()

namespaceSanityTest = hspec $ do
  describe "linux name space sanity tests.." $ do
    it "pid must be one for new PID NS" getPidNs

main :: IO ()
main = namespaceSanityTest
