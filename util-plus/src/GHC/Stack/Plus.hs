{-| Helper functions for dealing with call stacks and source locations.
 -}
module GHC.Stack.Plus
    ( callerLocation
    , callerFile
    , callerLine
    , adjustSourceFilePath
    )
where

import GHC.Stack
import Safe (tailSafe, lastNote)

adjustSourceFilePath :: FilePath -> FilePath
adjustSourceFilePath = tailSafe . dropWhile (/='/') . drop 1 . dropWhile (/= '/')

callerLocation :: (HasCallStack) => String
callerLocation = callerFile ++ ":" ++ show callerLine

-- | The filename of the first caller which called a function with implicit
-- parameter @(callStack :: 'CallStack')@.
callerFile :: (HasCallStack) => String
callerFile = srcLocFile . callerSrcLoc $ callStack

-- | The line number of the first caller which called a function with
-- implicit parameter @(callStack :: 'CallStack')@.
callerLine :: (HasCallStack) => Int
callerLine = srcLocStartLine . callerSrcLoc $ callStack

callerSrcLoc :: CallStack -> SrcLoc
callerSrcLoc = snd . caller

caller :: CallStack -> (String, SrcLoc)
caller = lastNote "Empty CallStack?!" . getCallStack
