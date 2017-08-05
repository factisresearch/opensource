{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.StrictVector.Mutable
    ( module Data.Vector.Generic.Mutable
    , MVector(..)
    , IOVector, STVector
    )
where

import Control.Monad.ST (RealWorld)
import Data.Vector.Generic.Mutable hiding (MVector)
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM

-- | 'MVector' is a strict wrapper around "Data.Vector.Mutable"'s 'Data.Vector.Mutable.MVector'
newtype MVector s a = MVector (VM.MVector s a)

instance VGM.MVector MVector a where
    basicLength (MVector v) = VGM.basicLength v
    basicUnsafeSlice n m (MVector v) = MVector $ VGM.basicUnsafeSlice n m v
    basicOverlaps (MVector v1) (MVector v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = fmap MVector (VGM.basicUnsafeNew n)
    basicInitialize (MVector v) = basicInitialize v
    basicUnsafeReplicate n x = x `seq` fmap MVector (VGM.basicUnsafeReplicate n x)
    basicUnsafeRead (MVector v) n = VGM.basicUnsafeRead v n
    basicUnsafeWrite (MVector v) n x = x `seq` VGM.basicUnsafeWrite v n x
    basicClear (MVector v) = VGM.basicClear v
    basicSet (MVector v) x = x `seq` VGM.basicSet v x
    basicUnsafeCopy (MVector v1) (MVector v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MVector v1) (MVector v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MVector v) n = fmap MVector (VGM.basicUnsafeGrow v n)

type IOVector = MVector RealWorld
type STVector s = MVector s

