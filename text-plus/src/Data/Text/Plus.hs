{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Data.Text.Plus
    ( module Data.Text
    , module Data.Text.Encoding
    , decodeUtf8M
    , showText
    , readText
    , groupOn
    , withoutTags
    , showTable
    , showTableRaw
    , filename
    , splitOnNoEmpty
    , nothingIfEmpty
    , noneIfEmpty
    , emptyIfNone
    , limitTo
    , sep, unsep, unsep'
    , shorten
    , shortenL
    , firstToUpper
    , shortenLinesL
    , lenientDecodeUtf8, lenientDecodeUtf8L
    , toLazy
    , fromLazy
    , indicesOfOccurences
    , tokenize
    , commonPrefixTotal
    , firstLine
    , firstParagraph
    , escapeXml
    , fixed
    , fixed'
    )
where

import Data.Fail
import Data.Option
import Safe.Plus

import Data.Char (isSpace)
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Text
import Data.Text.Encoding hiding (Decoding(Some))
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

fixed :: Int -> T.Text -> T.Text
fixed = fixed' '0'

fixed' :: Char -> Int -> T.Text -> T.Text
fixed' ch i s =
    let n = i - T.length s
    in T.replicate n (T.singleton ch) `T.append` s

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink t = T.pack <$> shrink (T.unpack t)

fromLazy :: TL.Text -> T.Text
fromLazy = TL.toStrict

toLazy :: T.Text -> TL.Text
toLazy = TL.fromStrict

showText :: Show a => a -> T.Text
showText = T.pack . show

showTextL :: Show a => a -> TL.Text
showTextL = TL.pack . show

readText :: Read a => T.Text -> a
readText = read . T.unpack

limitTo :: Int -> T.Text -> T.Text
limitTo lim t
    | lim <= 5 = t
    | T.length t <= lim = t
    | otherwise =
        T.take (lim - 4) t
        <> "..."
        <> T.drop (T.length t - 1) t

groupOn :: Eq a => (Char -> a) -> Text -> [(a, T.Text)]
groupOn _ "" = []
groupOn proj t = (x', x `T.cons` ys) : groupOn proj zs
    where
      x = T.head t
      xs = T.tail t
      x' = proj x
      (ys,zs) = T.span ((==x') . proj) xs

firstToUpper :: T.Text -> T.Text
firstToUpper t =
    case T.uncons t of
      Nothing -> ""
      Just (fstChr, rstText) ->
          C.toUpper fstChr `cons` rstText

-- |Removes HTML Tags
withoutTags :: T.Text -> T.Text
withoutTags =
    let betweenTags ('<':xs) = inTag xs
        betweenTags (x:xs) = x:betweenTags xs
        betweenTags [] = []
        inTag ('>':xs) = betweenTags xs
        inTag ('\'':xs) = inSingQuot xs
        inTag ('"':xs) = inDoubleQuot xs
        inTag (_:xs) = inTag xs
        inTag [] = [] -- incorrect HTML
        inSingQuot ('\'':xs) = inTag xs
        inSingQuot (_:xs) = inSingQuot xs
        inSingQuot [] = [] -- incorrect HTML
        inDoubleQuot ('\"':xs) = inTag xs
        inDoubleQuot (_:xs) = inDoubleQuot xs
        inDoubleQuot [] = [] -- incorrect HTML
    in T.pack . betweenTags . T.unpack

-- indicesOfOccurences needle haystack returns all indices i s.t.
--
-- prop> needle `T.isPrefixOf` (T.drop i haystack)
--
-- Note that: T.breakOnAll - does not return overlapping matches, e.g.
--
-- prop> indicesOfOccurences "edited" "editedited" == [0,4]
--
-- but
--
-- prop> map (T.length . fst) (T.breakOnAll "edited" "editedited") == [0]
--
indicesOfOccurences :: T.Text -> T.Text -> [Int]
indicesOfOccurences needle = go 0
    where
      go off haystack
          | Just (_, matchTail) <- T.uncons match = newOff:go (newOff+1) matchTail
          | otherwise = []
          where
            newOff = off + T.length prefix
            (prefix, match) = T.breakOn needle haystack

-- | Simple tokenizer - that doesn't destroy delimiters, e.g.
--
-- >>> tokenize (T.pack "This is blind-text, with punctuation.")
-- ["This"," ","is"," ","blind","-","text",", ","with"," ","punctuation","."]
--
-- prop> T.concat (tokenize x) == x
--
-- Note: URLs, numbers won't be handled very well.
tokenize :: T.Text -> [T.Text]
tokenize = T.groupBy ((==) `on` C.isAlphaNum)

-- | @commonPrefixTotal s t@ returns a trippel @(r,st,tt)@ s.t.
-- @
--    s = r `T.append` st, t = r `T.append` tt
-- @
-- such that @r@  is longest possible.
--
-- Note: Contrary to Data.commonPrefix there is no special case when @T.null r@.
commonPrefixTotal :: T.Text -> T.Text -> (T.Text, T.Text, T.Text)
commonPrefixTotal s t = fromMaybe ("", s, t) $ T.commonPrefixes s t

showTable :: (Traversable t) => [(T.Text, a -> T.Text)] -> t a -> T.Text
showTable headersAccessors rows =
    showTableRaw (fmap fst headersAccessors) (fmap (\x -> fmap (($ x) . snd) headersAccessors) rows)

showTableRaw :: (Traversable t1, Traversable t2) => [T.Text] -> t1 (t2 T.Text) -> T.Text
showTableRaw headers rows = table
  where
    rows' = fmap (fmap (wrap ' ')) rows
    columnHeaders =
        fmap (wrap ' ') headers
    header = renderRow columnHeaders
    headerBodySeperator =
        wrap '|' (T.intercalate "+" (fmap (`T.replicate` "-") fieldWidths))
    renderRow rowElems =
        wrap '|' (T.intercalate "|" (adjust (F.toList rowElems)))
    wrap char = T.cons char . (`T.snoc` char)
    table =
        flip T.snoc '\n' . T.intercalate "\n" $
        header : headerBodySeperator : F.toList (fmap renderRow rows')
    adjust = Prelude.zipWith (`T.justifyLeft` ' ') fieldWidths
    fieldWidths =
        Prelude.foldr (Prelude.zipWith (\a b -> max (T.length a) b))
            (F.toList (fmap T.length columnHeaders)) (fmap F.toList rows')

nothingIfEmpty :: T.Text -> Maybe T.Text
nothingIfEmpty t =
    if T.null $ T.strip t then Nothing else Just t

noneIfEmpty :: T.Text -> Option T.Text
noneIfEmpty = maybeToOption . nothingIfEmpty

emptyIfNone :: Option T.Text -> T.Text
emptyIfNone None = T.empty
emptyIfNone (Some t) = t

sep :: T.Text -> Char -> T.Text -> T.Text
sep prefix ch suffix
    | T.any (==ch) prefix =
        safeError ("Oh dear!  Won't separate `" ++ T.unpack prefix ++ "' with `" ++ show ch
                   ++ "' because it contains that character!")
    | otherwise = T.concat [prefix, T.singleton ch, suffix]

unsep' :: Monad m => Char -> T.Text -> m (T.Text, T.Text)
unsep' ch full =
    case T.span (/=ch) full of
      (prefix, T.uncons -> Just (ch', suffix)) | ch == ch' -> return (prefix, suffix)
      _ -> safeFail ("Can't unsep `" ++ T.unpack full ++ "' using `" ++ show ch ++ "'.")

unsep :: Char -> T.Text -> (T.Text, T.Text)
unsep ch x = safeFromOk (unsep' ch x)

shorten :: Int -> T.Text -> T.Text
shorten len = TL.toStrict . shortenL len . TL.fromStrict

shortenL :: Int -> TL.Text -> TL.Text
shortenL (fromIntegral -> maxLen) s =
    let actualLen = TL.length s
        skipMsg = TL.concat ["... (", showTextL (actualLen - maxLen), " more chars)"]
        skipMsgLen = TL.length skipMsg
    in if actualLen <= maxLen + skipMsgLen
          then s
          else TL.concat [TL.take maxLen s, skipMsg]

shortenLinesL :: Int -> Int -> TL.Text -> TL.Text
shortenLinesL maxLines maxLineLength (Prelude.map (shortenL maxLineLength) . TL.lines -> xs) =
    let actualLines = Prelude.length xs
        skipMsg = TL.concat ["(", showTextL (actualLines - maxLines), " more lines)"]
        lines
            | actualLines <= maxLines + 1 = xs
            | otherwise = Prelude.take maxLines xs ++ [skipMsg]
    in TL.unlines lines

filename :: T.Text -> T.Text
filename = T.replace "?" "_" . T.replace "/" "_" . T.replace "." "_" . T.replace " " "_"

splitOnNoEmpty :: T.Text -> T.Text -> [T.Text]
splitOnNoEmpty break t =
    Prelude.filter (/= "") $ T.splitOn break t

lenientDecodeUtf8 :: BS.ByteString -> T.Text
lenientDecodeUtf8 = decodeUtf8With TE.lenientDecode

lenientDecodeUtf8L :: BSL.ByteString -> TL.Text
lenientDecodeUtf8L = TLE.decodeUtf8With TE.lenientDecode

decodeUtf8M :: BS.ByteString -> Fail T.Text
decodeUtf8M bs =
    case decodeUtf8' bs of
      Left (TE.DecodeError err (Just w8)) ->
          Fail $
          "Failed decoding " ++ show bs ++ " as UTF-8 on character " ++ show w8 ++ ": " ++ err
      Left (TE.DecodeError err Nothing) ->
          Fail $ "Failed decoding " ++ show bs ++ " as UTF-8: " ++ err
      Left _ -> safeError "Never used according to documentation."
      Right txt -> Ok txt

firstParagraph :: T.Text -> T.Text
firstParagraph =
    T.unlines . Prelude.takeWhile (not . endOfParagraph) . T.lines
    where
      endOfParagraph = T.all isSpace

firstLine :: T.Text -> T.Text
firstLine = T.takeWhile (/='\n')

escapeXml :: T.Text -> T.Text
escapeXml = T.concatMap escape
    where
      escape c =
          case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            '"' -> "&quot;"
            '\'' -> "&apos;"
            c -> T.singleton c
