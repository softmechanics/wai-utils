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

fromRight def (Left _) = def
fromRight _ (Right r) = r

testWithLBS = test [
    "withLBS complete" ~: do
      res <- withLBSTester $ \lbs -> 
        return $ lbs == out
      assert $ fromRight False res
  , "withLBS parital" ~: do
      res <- withLBSTester $ \lbs -> do
        return $ "0" == (B8.unpack $ head $ L.toChunks lbs) 
      assert $ fromRight False res
  ]

