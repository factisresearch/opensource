module Test.Arbitrary where

import Test.QuickCheck
import Control.Monad (forM)
import Data.Core.Graph.NodeManager (NodeMap)
import Data.Core.Graph.PureCore (Graph, empty, fromAdj)
import qualified Data.IntMap.Strict as IM


newtype TestNodeMap v = TestNodeMap(NodeMap v) deriving Show

instance Arbitrary v => Arbitrary (TestNodeMap v) where
    arbitrary = fmap (TestNodeMap . IM.fromList . map (\(NonNegative i, x) -> (i, x))) arbitrary

instance Arbitrary Graph where
    arbitrary = frequency [(1, return empty), (20, denseGraph)]
        where denseGraph =
                do n <- choose (0, 30::Int)
                   let nodeList = [1..n]
                   adj <- forM nodeList $ \i ->
                            do bits <- vectorOf n arbitrary
                               return (i, [ x | (x,b) <- zip nodeList bits, b ])
                   return $ fromAdj adj
