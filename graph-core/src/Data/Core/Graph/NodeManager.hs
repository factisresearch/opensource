{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Data.Core.Graph.NodeManager
    ( NodeManager, Node, NodeMap, NodeSet
    , emptyNode
    , initNodeManager, emptyNodeManager, getNodeMap
    , getNodeHandle, getExistingNodeHandle, lookupNode, unsafeLookupNode
    , removeNodeHandle
    , getNewNodesSince, keys, hasKey, nodes, toList
    , isConsistent
    )
where

import Control.Monad.State.Strict
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L

type Node = Int
type NodeMap v = IM.IntMap v
type NodeSet = IS.IntSet

emptyNode :: Node
emptyNode = -1

data NodeManager k
    = NodeManager
    { nm_nodeToKey :: !(NodeMap k)
    , nm_keyToNode :: !(HM.HashMap k Node)
    , nm_nextNode :: !Node
    } deriving (Show, Eq)

swap :: forall a b. (a, b) -> (b, a)
swap (x,y) = (y,x)

isConsistent :: (Ord k) => NodeManager k -> Bool
isConsistent (NodeManager{..}) =
    IM.size nm_nodeToKey == HM.size nm_keyToNode
    && (IM.null nm_nodeToKey || (nm_nextNode > fst (IM.findMax nm_nodeToKey)
                              && emptyNode < fst (IM.findMin nm_nodeToKey)))
    && L.sort (HM.toList nm_keyToNode) == L.sort (map swap (IM.toList nm_nodeToKey))

-- map must contain only non-negative keys!
initNodeManager :: (Hashable k, Eq k) => NodeMap k -> NodeManager k
initNodeManager nm =
    case IM.minViewWithKey nm of
       Just ((n, _), _) | n <= emptyNode -> error $ "Invalid node ID: " ++ show n
       _ -> NodeManager nm (invert nm) nextNode
    where nextNode
            | IM.null nm = 0
            | otherwise = 1 + fst (IM.findMax nm)
          invert im = HM.fromList . map swap $ IM.toList im

getNodeMap :: (Hashable k, Eq k) => NodeManager k -> NodeMap k
getNodeMap = nm_nodeToKey

keys :: NodeManager k -> [k]
keys nm =
    HM.keys (nm_keyToNode nm)

hasKey :: (Eq k, Hashable k) => k -> NodeManager k -> Bool
hasKey k nm =
    isJust $ HM.lookup k (nm_keyToNode nm)

toList :: NodeManager k -> [(k, Node)]
toList nm = HM.toList (nm_keyToNode nm)

nodes :: NodeManager k -> [Node]
nodes nm = IM.keys (nm_nodeToKey nm)

getNewNodesSince :: Node -> NodeManager k -> NodeMap k
getNewNodesSince n (NodeManager{..}) = snd $ IM.split n nm_nodeToKey

emptyNodeManager :: forall k. NodeManager k
emptyNodeManager = NodeManager IM.empty HM.empty 0

getNodeHandle :: (Hashable k, Eq k, MonadState (NodeManager k) m) => k -> m Node
getNodeHandle k =
    do NodeManager{..} <- get
       case HM.lookup k nm_keyToNode of
          Just i -> return i
          Nothing ->
            do let i = nm_nextNode
               put $! NodeManager { nm_nodeToKey = IM.insert i k nm_nodeToKey
                                  , nm_keyToNode = HM.insert k i nm_keyToNode
                                  , nm_nextNode = i + 1
                                  }
               return i

removeNodeHandle :: (Hashable k, Eq k) => Node -> NodeManager k -> NodeManager k
removeNodeHandle i nm@(NodeManager{..}) =
    case IM.lookup i nm_nodeToKey of
      Just k ->
          nm { nm_nodeToKey = IM.delete i nm_nodeToKey
             , nm_keyToNode = HM.delete k nm_keyToNode
             }
      Nothing -> nm

getExistingNodeHandle :: (Hashable k, Eq k) => k -> NodeManager k -> Maybe Node
getExistingNodeHandle k (NodeManager{..}) = HM.lookup k nm_keyToNode

lookupNode :: Node -> NodeManager k -> Maybe k
lookupNode i (NodeManager{..}) = IM.lookup i nm_nodeToKey

unsafeLookupNode :: Node -> NodeManager k -> k
unsafeLookupNode i nm = fromJust $ lookupNode i nm
