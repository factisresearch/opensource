{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Preview
    (Preview(..), preview, previewNamedSet, previewNamedList,  previewList
    ,previews, previewRec ,previewRec', previewKv, showKv
    ,previewElems, previewsElems, pprMapping, previewList', previewsPrecMapping
    ,Ppr(..), Ppr'(..), Doc, pretty, prettyText, docFromStr, shortPreviewStr
    ,docFromText,angles
    )
where

import Data.Choice (Choice(..))
import Data.Fail (Fail(..), pattern Fail)
import Data.List.Plus
import Data.Option (Option(..))
import Data.StrictList (StrictList, toLazyList)
import Data.StrictTuple (Pair(..))

import Data.Int (Int32, Int64)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Word (Word8, Word64)
import Text.PrettyPrint.HughesPJ (Doc, (<>), (<+>))
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.PrettyPrint.HughesPJ as P

_LIST_PREVIEW_ELEMS_ :: Int
_LIST_PREVIEW_ELEMS_ = 10

-- | Conversion of values to short readable strings.
-- Preview allows defining short and readable representations of potentially huge data structures
-- that can be used in logs for example. E.g. the Preview instance for lists may only print the
-- values at the beginning of the list and omit the rest.
class Preview a where
    -- | Create a preview String for the given value.
    --
    -- 'previewsPrec' should satisfy the law
    --
    -- > previewsPrec d x r ++ s == previewsPrec d x (r ++ s)
    previewsPrec
        :: Int
        -- ^ The operator precedence of the enclosing context (a number from 0 to 11). Function
        -- application has precedence 10.
        -> a -- ^ the value to be previewed
        -> String -- ^ the string to be appended at the end of the output (for constant time append)
        -> String

instance Preview () where previewsPrec = showsPrec
instance Preview Char where previewsPrec = showsPrec
instance Preview Int where previewsPrec = showsPrec
instance Preview Bool where previewsPrec = showsPrec
instance Preview a => Preview [a] where previewsPrec = previewList
instance Preview T.Text where  previewsPrec = previewsText

instance (Preview a, Preview b) => Preview (Either a b) where
    previewsPrec prec eAB s =
        case eAB of
          Left a -> previewsPrec prec a s
          Right b -> previewsPrec prec b s

instance (Preview a) => Preview (Maybe a) where
    previewsPrec prec mA s =
        case mA of
          Nothing -> showString "Nothing" s
          Just a -> previewsPrec prec a s

instance (Preview a, Preview b) => Preview (a, b) where
    previewsPrec prec (a, b) =
        showParen (prec >= 10) $
        previewsPrec 5 a .
        showString ", " .
        previewsPrec 5 b

instance (Preview a, Preview b, Preview c) => Preview (a, b, c) where
    previewsPrec prec (a, b, c) =
        showString "(" .
        previewsPrec prec a .
        showString ", " .
        previewsPrec prec b .
        showString ", " .
        previewsPrec prec c .
        showString ")"

instance Preview Word8 where
    previewsPrec = showsPrec

instance Preview Word64 where
    previewsPrec = showsPrec

instance Preview Int32 where
    previewsPrec = showsPrec

instance Preview Int64 where
    previewsPrec = showsPrec

instance (Preview k, Preview v) => Preview (Map k v) where
    previewsPrec p = previewsPrecMapping p . Map.toList

instance Preview a => Preview (Fail a) where
    previewsPrec p x =
        case x of
          Fail msg -> showParen (p > 10) $ showString "Fail " . showsPrec 5 msg
          Ok a -> previewsPrec p a

instance (Preview a, Preview b) => Preview (Pair a b) where
    previewsPrec p (x :!: y) = previewsPrec p (x, y)

instance Preview a => Preview (Option a) where
    previewsPrec prec mA s =
        case mA of
          None -> showsPrec prec "None" s
          Some a -> previewsPrec prec a s

instance Preview a => Preview (StrictList a) where
    previewsPrec x y = previewList x (toLazyList y)

instance (Preview a, Preview b) => Preview (Choice a b) where
    previewsPrec p choice =
        case choice of
          This a -> previewsPrec p a
          That b -> previewsPrec p b

previewsPrecMapping :: (Preview k, Preview v) => t -> [(k, v)] -> String -> String
previewsPrecMapping _ =
        (showString "{ " .) .
        foldr (.) (showString "}") .
        intersperse (showString "\n, ") .
        map (\(k,xs) -> previewsPrec 10 k . showString " -> " . previewsPrec 10 xs) .
        take _LIST_PREVIEW_ELEMS_

class Ppr a where
    ppr :: a -> Doc
    pprMany :: Foldable f => f a -> Doc
    pprMany xs = P.brackets (P.sep $ P.punctuate P.comma $ fmap ppr (F.toList xs))

class Ppr' k where
    ppr' :: Ppr a => k a -> Doc

instance Ppr () where
    ppr () = P.text "()"

instance Ppr Int64 where
    ppr i = P.text (show i)

instance Ppr Char where
    ppr = P.char
    pprMany xs = P.char '"' <> F.foldl' (\d x -> d <> ppr x) P.empty xs <> P.char '"'

instance Ppr Bool where
    ppr x = if x then P.text "True" else P.text "False"

instance Ppr a => Ppr [a] where
    ppr = pprMany

instance Ppr a => Ppr (Set a) where
    ppr = pprMany

instance (Ppr a, Ppr b) => Ppr (Map a b) where
    ppr = pprMapping . Map.toList

instance Ppr Doc where
    ppr = id

instance Ppr a => Ppr (Fail a) where
    ppr (Ok x) = "Ok" <+> ppr x
    ppr (Fail msg) = "Fail" <+> docFromStr (show msg)

pprMapping :: (Ppr a, Ppr b) => [(a, b)] -> Doc
pprMapping xs =
    P.braces (P.sep $ P.punctuate P.comma $ fmap pprTuple xs)
    where
      pprTuple (a, b) = P.sep [ppr a <+> P.text "->", P.nest 4 $ ppr b]

instance (Ppr a, Ppr b) => Ppr (a, b) where
    ppr (a, b) = P.parens (ppr a <> P.comma <+> ppr b)

instance Ppr Int where
    ppr = docFromStr . show

instance Ppr Integer where
    ppr = docFromStr . show

instance Ppr Int32 where
    ppr = docFromStr . show

instance Ppr Word8 where
    ppr = docFromStr . show

instance Ppr Double where
    ppr = docFromStr . show

instance Ppr a => Ppr (Maybe a) where
    ppr (Just x) = ppr x
    ppr Nothing = P.text "Nothing"

instance Ppr Word64 where
    ppr = docFromStr . show

pretty :: Ppr a => a -> String
pretty = P.renderStyle (P.style { P.mode = P.LeftMode }) . ppr

prettyText :: Ppr a => a -> T.Text
prettyText = T.pack . pretty

docFromStr :: String -> Doc
docFromStr = P.text

docFromText :: T.Text -> Doc
docFromText = P.text . T.unpack

preview :: Preview a => a -> String
preview x = previewsPrec 5 x ""

previews :: Preview a => a -> String -> String
previews = previewsPrec 0

previewRec :: Int -> String -> [(String, Int -> String -> String)] -> String -> String
previewRec prec tyName fields =
    previewRec' prec tyName (map mapField fields)
    where
      mapField (n,f) = showString n . showString "=" . f 11

previewRec' :: Int -> String -> [String -> String] -> String -> String
previewRec' prec tyName fields =
    showParen (prec > 10) $
    showString tyName .
    showString " { " .
    foldl' (.) id (intersperse (showString ", ") fields) .
    showString " }"

previewKv :: Preview a => String -> a -> String -> String
previewKv name x =
    showString name . showString "=" . (previewsPrec 5 x)

showKv :: Show a => String -> a -> String -> String
showKv name x =
    showString name . showString "=" . (showsPrec 5 x)

previewNamedSet :: String -> t -> Set a -> String -> String
previewNamedSet name _prec set =
    showString "|" . showString name . showString "|=" . showString (show (Set.size set))

previewNamedList :: String -> t -> [a] -> String -> String
previewNamedList name _prec xs =
    showString "|" . showString name . showString "|=" . showString (show (length xs))

previewList' :: Preview a => Maybe Int -> Int -> [a] -> String -> String
previewList' maxElems _prec xs =
    case xs of
      [] -> showString "[]"
      (x:rest)
          | maxElems == Nothing || length xs <= fromMaybe 0 maxElems ->
              showString "[" .
              foldl' (.) id (intersperse (showString ", ") (map (previewsPrec 11) xs)) .
              showString "]"
          | otherwise ->
              showString "[" .
              previewsPrec 5 x .
              if null rest
                 then showString "]"
                 else showString "... " .
                      showsPrec 5 (length rest) .
                      showString " more elems)]"

previewList :: Preview a => Int -> [a] -> String -> String
previewList = previewList' (Just _LIST_PREVIEW_ELEMS_)

previewsElems :: (Foldable f, Preview a) => Int -> f a -> String -> String
previewsElems _prec xs =
    showString "[" .
    foldl' (.) id (intersperse (showString ", ") (map (previewsPrec 5) (F.toList xs))) .
    showString "]"

previewElems :: (Foldable t, Preview a) => t a -> String
previewElems xs = previewsElems 5 xs ""

previewsText :: Int -> T.Text -> ShowS
previewsText _ t
    | T.length t < 65 = showString (T.unpack t)
    | otherwise = showString (T.unpack (T.take 65 t)) . showString "..."

shortPreviewStr :: Int -> String -> String
shortPreviewStr n s =
    if length s <= n
    then s
    else take n s ++ "..."

angles :: Doc -> Doc
angles p = P.char '<' <> p <> P.char '>'


