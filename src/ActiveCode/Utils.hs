module ActiveCode.Utils (readCommand) where
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, mask, try, onException, throwIO)
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad
import System.IO
import System.Process
import System.Exit      ( ExitCode(..) )

import GHC.IO.Exception ( IOErrorType(..), IOException(..) )
import Foreign.C

readCommand
    :: FilePath             -- ^ Filename of the executable (see 'proc' for details)
    -> [String]             -- ^ any arguments
    -> String               -- ^ standard input
    -> IO (ExitCode,String) -- ^ exitcode, stdout+stderr
readCommand proc args input = do
    let cmd = unwords $ proc : (args ++ ["2>&1"])
    mask $ \restore -> do
      (inh, outh, _, pid) <- runInteractiveCommand cmd
      flip onException
        (do hClose inh; hClose outh; 
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- hGetContents outh
        waitOut <- forkWait $ C.evaluate $ rnf out

        -- now write and flush any input
        let writeInput = do
              unless (null input) $ do
                hPutStr inh input
                hFlush inh
              hClose inh

        C.catch writeInput $ \e -> case e of
          IOError { ioe_type = ResourceVanished
                  , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
          _ -> throwIO e

        -- wait on the output
        waitOut

        hClose outh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)
