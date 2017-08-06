{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Safe.Plus
    ( callerFile
    , callerLine
    , callerLocation
    , safeDigitToInt
    , safeRead
    , readNoteVerbose
    , safeFromJust
    , safeFromJustNote
    , safeHead
    , safeTail
    , safeInit
    , safeLast
    , safeMaximum
    , safeMinimum
    , safeHeadNote
    , safeFromRight
    , fromRightNote
    , safeFromLeft
    , fromLeftNote
    , safeAtArray
    , atArrayNote
    , safeAt
    , safeError
    , safeFail
    , safeUndef
    )
where

import Data.Array.IArray
import Data.Char
import Data.Monoid
import GHC.Stack
import GHC.Stack.Plus
import Safe

-- | Convert a single digit 'Char' to the corresponding 'Int'.
-- This function fails unless its argument satisfies 'isHexDigit',
-- but recognises both upper and lower-case hexadecimal digits
-- (i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@).
safeDigitToInt :: Monad m => Char -> m Int
safeDigitToInt c
    | isDigit c  = return $ ord c - ord '0'
    | c >= 'a' && c <= 'f' = return $ ord c - ord 'a' + 10
    | c >= 'A' && c <= 'F' = return  $ ord c - ord 'A' + 10
    | otherwise = safeFail ("Char.safeDigitToInt: not a digit " ++ show c)

safeUndef :: (HasCallStack) => a
safeUndef = safeError "undefined!"

safeRead :: (HasCallStack, Read a) => String -> a
safeRead = readNoteVerbose callerLocation

readNoteVerbose :: Read a => String -> String -> a
readNoteVerbose msg s =
    case [x | (x,t) <- reads s, ("","") <- lex t] of
      [x] -> x
      []  -> error $ "Prelude.read: no parse, " ++ msg ++ ", on " ++ prefix
      _   -> error $ "Prelude.read: ambiguous parse, " ++ msg ++ ", on " ++ prefix
    where
        prefix = '\"' : a ++ if null b then "\"" else "..."
            where (a,b) = splitAt 1024 s

safeFromJust :: (HasCallStack) => Maybe a -> a
safeFromJust = Safe.fromJustNote callerLocation

safeFromJustNote :: (HasCallStack) => String -> Maybe a -> a
safeFromJustNote s = Safe.fromJustNote (callerLocation ++ ": " ++ s)

safeFail :: (HasCallStack, Monad m) => String -> m a
safeFail x = fail (callerLocation ++ ": FAIL: " ++ x)

safeHead :: (HasCallStack) => [a] -> a
safeHead = Safe.headNote callerLocation

safeTail :: (HasCallStack) => [a] -> [a]
safeTail = Safe.tailNote callerLocation

safeInit :: (HasCallStack) => [a] -> [a]
safeInit = Safe.initNote callerLocation

safeLast :: (HasCallStack) => [a] -> a
safeLast = Safe.lastNote callerLocation

safeMaximum :: (HasCallStack, Ord a) => [a] -> a
safeMaximum = Safe.maximumNote callerLocation

safeMinimum :: (HasCallStack, Ord a) => [a] -> a
safeMinimum = Safe.minimumNote callerLocation

safeHeadNote :: (HasCallStack) => String -> [a] -> a
safeHeadNote x = Safe.headNote (callerLocation ++ ": " ++ x)

safeFromRight :: (HasCallStack) => Either a b -> b
safeFromRight = fromRightNote callerLocation

fromRightNote :: String -> Either a b -> b
fromRightNote msg (Left _) = error $ "fromRight got a left value: " ++ msg
fromRightNote _ (Right x) = x

safeFromLeft :: (HasCallStack) => Either a b -> a
safeFromLeft = fromLeftNote callerLocation

fromLeftNote :: String -> Either a b -> a
fromLeftNote msg (Right _) = safeError $ "fromLeft got a right value: " ++ msg
fromLeftNote _ (Left x) = x

safeAtArray :: (HasCallStack, IArray a e, Ix i, Show i) => a i e -> i -> e
safeAtArray = atArrayNote callerLocation

atArrayNote :: (IArray a e, Ix i, Show i) => String -> a i e -> i -> e
atArrayNote msg array index
    | inRange arrayBounds index = array ! index
    | otherwise =
        error $ concat
            [ "lookup at index ", show index, " in an array was outside of its bounds "
            , show arrayBounds, ": ", msg
            ]
    where
      arrayBounds = bounds array

safeAt :: (HasCallStack) => [a] -> Int -> a
safeAt = Safe.atNote callerLocation

safeError :: (HasCallStack) => String -> a
safeError err = error $ callerLocation <> ": " <> err
