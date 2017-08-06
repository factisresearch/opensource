{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Core.Graph.PureCore where

import Data.Core.Graph.NodeManager hiding (nodes, isConsistent)

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.Identity
import Control.Monad.ST
import Data.Function (on)
import Data.Hashable
import Data.Maybe
import Data.STRef
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU

type AdjList = NodeMap (VU.Vector Node)
data Edge = Edge { src :: !Node, tgt :: !Node } deriving (Show, Eq, Ord)

instance Hashable Edge where
    hashWithSalt s (Edge x y) = s `hashWithSalt` x `hashWithSalt` y

data Graph
    = Graph
    { g_adj :: !AdjList
    , g_radj :: !AdjList
    }

empty :: Graph
empty = Graph IM.empty IM.empty

invert :: Edge -> Edge
invert (Edge x y) = Edge y x

instance Show Graph where
    show g = "< " ++ L.intercalate ",\n  " (map showNode (nodes g)) ++ " >"
        where showNode x = show x
                        ++ " -> ["
                        ++ L.intercalate "," (map show (VU.toList (children g x)))
                        ++ "]"

instance Eq Graph where
    a == b = sameItems (nodes a) (nodes b)
          && all (\x -> sameItems (VU.toList (children a x)) (VU.toList (children b x)))
                  (nodes a)
      where sameItems x y = IS.fromList x == IS.fromList y

instance NFData Graph where
    rnf (Graph a b) = rnf a `seq` rnf b

adjToEdges :: [(Node, [Node])] -> [Edge]
adjToEdges = concatMap (\(x, ys) -> map (Edge x) ys)

edgesAdj :: AdjList -> [Edge]
edgesAdj adj = adjToEdges . map (second VU.toList) $ IM.toList adj

isConsistent :: Graph -> Bool
isConsistent (Graph{..}) = L.sort forwardEdges == L.sort (map invert (edgesAdj g_radj))
                        && HS.size (HS.fromList forwardEdges) == length forwardEdges
    where forwardEdges = edgesAdj g_adj

fromEdges :: [Edge] -> Graph
fromEdges edgeList =
    Graph { g_adj = mkAdj edgeList
          , g_radj = mkAdj $ map invert edgeList
          }
    where
      mkAdj e = IM.fromList $ map (src . head &&& VU.fromList . map tgt)
                        . L.groupBy ((==) `on` src)
                        . L.sortBy (compare `on` src) $ e

fromAdj :: [(Node, [Node])] -> Graph
fromAdj l =
    let g1 = fromEdges (adjToEdges l)
        solitaires = map fst $ filter (\(_, xs) -> null xs) l
    in L.foldl' (\g n -> g { g_adj = IM.insert n VU.empty (g_adj g) }) g1 solitaires

nodes :: Graph -> [Node]
nodes g = IM.keys (IM.union (g_adj g) (g_radj g))

edges :: Graph -> [Edge]
edges = edgesAdj . g_adj

solitaireNodes :: Graph -> [Node]
solitaireNodes g = IM.keys (IM.filter VU.null (IM.union (g_adj g) (g_radj g)))

edgeCount :: Graph -> Int
edgeCount = F.foldl' (\old (_,adj) -> old + VU.length adj) 0
          . IM.toList . g_adj

children :: Graph -> Node -> VU.Vector Node
children g x = neighbors g (g_adj g) x

parents :: Graph -> Node -> VU.Vector Node
parents g x = neighbors g (g_radj g) x

neighbors :: Graph -> AdjList -> Node -> VU.Vector Node
neighbors (Graph{..}) adj x = IM.findWithDefault VU.empty x adj

hasEdge :: Node -> Node -> Graph -> Bool
hasEdge x y (Graph{..}) = y `VU.elem` IM.findWithDefault VU.empty x g_adj

addNode :: Node -> Graph -> Graph
addNode x g =
    g { g_adj = IM.insertWith (\_new old -> old) x VU.empty (g_adj g) }

removeNode :: Node -> Graph -> Graph
removeNode x g =
    let rmInAdj adj localF =
            foldl (\adjList child ->
                       IM.adjust (VU.filter (/=x)) child adjList
                  ) (IM.delete x adj) $ VU.toList (localF g x)

        newAdj = rmInAdj (g_adj g) parents
        newRAdj = rmInAdj (g_radj g) children
    in g { g_adj = newAdj
         , g_radj = newRAdj
         }

addEdge :: Node -> Node -> Graph -> Graph
addEdge x y g@(Graph{..}) =
   if hasEdge x y g
       then g
       else Graph { g_adj = alterDef VU.empty (flip VU.snoc y) x g_adj
                  , g_radj = alterDef VU.empty (flip VU.snoc x) y g_radj
                  }
   where alterDef def f = IM.alter (Just . f . fromMaybe def)

addEdges :: [Edge] -> Graph -> Graph
addEdges edgeList g = L.foldl' (flip (\(Edge x y) -> addEdge x y)) g edgeList

removeEdge :: Node -> Node -> Graph -> Graph
removeEdge x y (Graph{..}) =
    Graph { g_adj = IM.adjust (VU.filter (/=y)) x g_adj
          , g_radj = IM.adjust (VU.filter (/=x)) y g_radj
          }

removeEdges :: [Edge] -> Graph -> Graph
removeEdges edgeList g = L.foldl' (flip (\(Edge x y) -> removeEdge x y)) g edgeList

hull :: Graph -> Node -> NodeSet
hull g = hullImpl g (g_adj g)

rhull :: Graph -> Node -> NodeSet
rhull g = hullImpl g (g_radj g)

hullImpl :: Graph -> AdjList -> Node -> NodeSet
hullImpl (Graph{..}) adj root =
    runST $
       do vis <- newSTRef IS.empty
          let go x =
               (IS.member x <$> readSTRef vis) >>=
                  (flip unless $
                     do modifySTRef' vis (IS.insert x)
                        VU.forM_ (IM.findWithDefault VU.empty x adj) go)
          go root
          readSTRef vis

rhullFold :: Graph -> (b -> Node -> b) -> b -> Node -> b
rhullFold g f initial node =
    runIdentity $ hullFoldImpl (g_radj g) (\x y -> return (f x y)) initial node

-- FIXME: benchmark against old hullFold implementation
hullFold :: Graph -> (b -> Node -> b) -> b -> Node -> b
hullFold g f initial node =
    runIdentity $ hullFoldImpl (g_adj g) (\x y -> return (f x y)) initial node

hullFoldM :: Monad m => Graph -> (b -> Node -> m b) -> b -> Node -> m b
hullFoldM g = hullFoldImpl (g_adj g)

hullFoldImpl :: Monad m => AdjList -> (b -> Node -> m b) -> b -> Node -> m b
hullFoldImpl adj f initial root =
    go IS.empty initial [root]
    where
      go _ acc [] = return acc
      go !visited !acc (x:xs) =
          if (IS.member x visited)
          then go visited acc xs
          else do newAcc <- f acc x
                  let succs = IM.findWithDefault VU.empty x adj
                  go (IS.insert x visited) newAcc (xs ++ VU.toList succs)
