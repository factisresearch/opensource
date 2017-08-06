{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Persistence where

import Data.Core.Graph
import Data.Core.Graph.NodeManager
import Data.Core.Graph.Persistence

import Test.Arbitrary
import Test.Framework

prop_persistence :: TestNodeMap Char -> Graph -> Bool
prop_persistence (TestNodeMap nodeMap) graph =
    let nodeMgr = initNodeManager nodeMap
        (nodeMgr', graph') = loadGraph (persistGraph nodeMgr graph)
    in (nodeMgr' == nodeMgr && graph == graph')
