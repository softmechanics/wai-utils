module Network.Wai.Util (
    withLBS
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (($$))
import qualified Data.Enumerator as E
import Data.IORef
import Network.Wai
import System.IO.Unsafe

withLBS :: (L.ByteString -> IO a) -> E.Iteratee B.ByteString IO a
withLBS f = do
  (ialive, mbs, mres) <- liftIO $ do
    mbs <- newEmptyMVar
    mres <- newEmptyMVar
    ialive <- newIORef True
    forkIO $ finally 
      (do a <- f =<< evalLBS mbs
          a `seq` putMVar mres a) 
      (writeIORef ialive False)
    return (ialive, mbs, mres)
  iterateLBS ialive mbs
  liftIO $ takeMVar mres

evalLBS :: MVar [B.ByteString] -> IO L.ByteString
evalLBS mbs = fmap L.fromChunks go
  where go = unsafeInterleaveIO $ do 
                next <- takeMVar mbs
                case next of
                     [] -> return []
                     bs -> fmap (bs ++) go

iterateLBS :: IORef Bool -> MVar [B.ByteString] -> E.Iteratee B.ByteString IO ()
iterateLBS ialive mbs = E.continue go
  where go (E.Chunks []) = E.continue go

        go (E.Chunks cs) = do 
          succ <- liftIO $ tryPut cs
          if succ
             then E.continue go
             else E.yield () $ E.Chunks cs

        go E.EOF = do 
          liftIO $ tryPut []
          E.yield () E.EOF

        tryPut cs = do 
          succ <- tryPutMVar mbs cs
          if succ 
             then return True
             else waitPut cs

        waitPut cs = do
          alive <- readIORef ialive
          if alive
             then yield >> tryPut cs
             else return False

