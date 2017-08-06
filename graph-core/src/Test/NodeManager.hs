{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.NodeManager where

import Data.Core.Graph.NodeManager

import Test.Arbitrary
import Test.Framework
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L

assertConsistent :: StateT (NodeManager Char) IO ()
assertConsistent = get >>= liftIO . assertEqual True . isConsistent

prop_init :: TestNodeMap String -> Property
prop_init (TestNodeMap m) = uniqueValues m && all (>=0) (IM.keys m)
        ==> isConsistent new && m == getNodeMap new
    where new = initNodeManager m
          uniqueValues im = IM.size im == length (L.nub $ IM.elems im)

test_getNewNodesSince :: IO ()
test_getNewNodesSince =
    flip evalStateT emptyNodeManager $
        do _ <- getNodeHandle 'a'
           n2 <- getNodeHandle 'b'
           n3 <- getNodeHandle 'c'
           n4 <- getNodeHandle 'd'
           new <- gets (getNewNodesSince n2)
           liftIO $ assertEqual (IM.fromList [(n3, 'c'), (n4, 'd')]) new

test_getNodeHandle :: IO ()
test_getNodeHandle =
    flip evalStateT emptyNodeManager $
        do n1 <- getNodeHandle 'a'
           n2 <- getNodeHandle 'a'
           liftIO $ assertEqual n1 n2
           n3 <- getNodeHandle 'b'
           n4 <- getNodeHandle 'b'
           liftIO $ assertEqual n3 n4
           n5 <- getNodeHandle 'a'
           liftIO $ assertEqual n1 n5
           liftIO $ assertNotEqual n1 n3
           assertConsistent

test_lookupNode :: IO ()
test_lookupNode =
    flip evalStateT emptyNodeManager $
        do n1 <- getNodeHandle 'a'
           n2 <- getNodeHandle 'b'
           x1 <- gets $ lookupNode n1
           x2 <- gets $ lookupNode n2
           x3 <- gets $ lookupNode 123
           liftIO $ assertEqual (Just 'a') x1
           liftIO $ assertEqual (Just 'b') x2
           liftIO $ assertEqual Nothing x3
           assertConsistent
