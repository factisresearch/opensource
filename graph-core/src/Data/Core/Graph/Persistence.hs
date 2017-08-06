module Data.Core.Graph.Persistence
  ( PersistentGraph, persistGraph, loadGraph )
where

import Data.Core.Graph.PureCore
import Data.Core.Graph.NodeManager

import Data.Hashable
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as VU

data PersistentGraph k
   = PersistentGraph
   { pg_nodeData :: NodeMap k
   , pg_graphData :: [(Node, [Node])]
   } deriving (Show, Eq)

persistGraph :: (Eq k, Hashable k) => NodeManager k -> Graph -> PersistentGraph k
persistGraph nodeManager graph =
    PersistentGraph
    { pg_nodeData = getNodeMap nodeManager
    , pg_graphData = map (\(k, vals) -> (k, VU.toList vals)) (IM.toList $ g_adj graph)
    }

loadGraph :: (Eq k, Hashable k) => PersistentGraph k -> (NodeManager k, Graph)
loadGraph (PersistentGraph nodeData graphData) =
    (initNodeManager nodeData, fromAdj graphData)