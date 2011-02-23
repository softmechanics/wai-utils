module WithLBS where

import Network.Wai.Util
import Test.HUnit
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as E

src  = map B8.pack [show i | i <- [0 .. 10000]]
out  = L.fromChunks src

withLBSTester f = do
  let enum = E.enumList 2 src
  E.run_ (enum $$ withLBS f)

testWithLBS = test [
    "withLBS complete" ~: do
      b <- withLBSTester $ \lbs -> 
        return $ lbs == out
      assert b
  , "withLBS parital" ~: do
      b <- withLBSTester $ \lbs -> do
        return $ "0" == (B8.unpack $ head $ L.toChunks lbs) 
      assert b
  ]

