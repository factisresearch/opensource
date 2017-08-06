{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.PlusSpec (htf_thisModulesTests) where

import Data.Text.Plus

import Test.Framework
import qualified Data.Char as C
import qualified Data.Text as T

test_groupOn :: IO ()
test_groupOn =
    do assertEqual [] $ groupOn id ""
       assertEqual [(False, "abc"), (True, "123"), (False, "def")] $ groupOn C.isDigit "abc123def"

test_firstToUpper :: IO ()
test_firstToUpper =
    do assertEqual "Hallo" $ firstToUpper "hallo"
       assertEqual "HAllo" $ firstToUpper "HAllo"
       assertEqual "" $ firstToUpper ""

test_withoutTags :: IO ()
test_withoutTags =
    do assertEqual "Hello laboratory report!"
                   (withoutTags $ "<p class=\"x\">Hello <pre>laboratory</pre> report!</p>")
       assertEqual "Hello laboratory report!"
                   (withoutTags $ "<p class=\"x>x<\">Hello <pre>laboratory</pre> report!</p>")
       assertEqual "Hello report!"
                   (withoutTags $ "<p class='s>x<'>Hello report!</p>")

prop_indicesOverlapping :: Property
prop_indicesOverlapping = once $ prop_indices "aba" "ababa"

prop_indicesThree :: Property
prop_indicesThree = once $ prop_indices "a" "ababa"

prop_indicesNone :: Property
prop_indicesNone = once $ prop_indices "foo" "ababa"

prop_indicesSingle :: Property
prop_indicesSingle = once $ prop_indices "bab" "ababa"

prop_indices :: T.Text -> T.Text -> Property
prop_indices n h = not (T.null n) ==>
    (indicesOfOccurences n h === [ i | i <- [0..(T.length h - 1)], n `T.isPrefixOf` (T.drop i h)])

test_tokenize :: IO ()
test_tokenize =
    assertEqual ["This"," ","is"," ","blind","-","text",", ","with", " ", "punctuation", "."] $
        tokenize "This is blind-text, with punctuation."

test_tokenizeWithNum :: IO ()
test_tokenizeWithNum =
    assertEqual ["100"," ","sheep"] $ tokenize "100 sheep"

test_tokenizeEmpty :: IO ()
test_tokenizeEmpty =
    assertEqual [] $ tokenize ""

test_showTableRaw :: IO ()
test_showTableRaw =
    let header = ["Col1", "Col2", "Col3"]
        table = [["longfield", "-", ""], ["short", "longfield", ""]]
    in assertEqual (showTableRaw header table) $
       T.concat
           [ "| Col1      | Col2      | Col3 |\n"
           , "|-----------+-----------+------|\n"
           , "| longfield | -         |      |\n"
           , "| short     | longfield |      |\n"
           ]

test_shortenL :: IO ()
test_shortenL =
    do assertEqual "123456789012345678901" (shortenL 20 "123456789012345678901")
       assertEqual "123456789012345678901" (shortenL 21 "123456789012345678901")
       assertEqual "123456789012345678901" (shortenL 22 "123456789012345678901")
       assertEqual "123456789012345678901" (shortenL 2 "123456789012345678901")
       assertEqual "1... (20 more chars)" (shortenL 1 "123456789012345678901")
       assertEqual "... (21 more chars)" (shortenL 0 "123456789012345678901")

test_shortenLinesL1 :: IO ()
test_shortenLinesL1 =
    assertEqual outp (shortenLinesL 2 1 inp)
    where
      outp =
          "1... (20 more chars)\n\
          \1... (20 more chars)\n\
          \(4 more lines)\n"
      inp =
          "123456789012345678901\n123456789012345678901\n123456789012345678901\n\
          \123456789012345678901\n123456789012345678901\n123456789012345678901"

test_shortenLinesL2 :: IO ()
test_shortenLinesL2 =
    assertEqual outp (shortenLinesL 0 0 inp)
    where
      outp = "(6 more lines)\n"
      inp =
          "123456789012345678901\n123456789012345678901\n123456789012345678901\n\
          \123456789012345678901\n123456789012345678901\n123456789012345678901"

test_filename :: IO ()
test_filename =
    do assertEqual "Foo" (filename "Foo")
       assertEqual "Foo_Bar" (filename "Foo Bar")
       assertEqual "Foo_Bar" (filename "Foo/Bar")
       assertEqual "Foo_Bar" (filename "Foo.Bar")
       assertEqual "Foo_Bar" (filename "Foo?Bar")

test_sepUnsep :: IO ()
test_sepUnsep =
    do assertEqual ("foo|bar") (sep "foo" '|' "bar")
       assertEqual ("foo", "bar") (unsep '|' "foo|bar")
       assertEqual ("foo", "bar|baz") (unsep '|' "foo|bar|baz")

test_firstLine :: IO ()
test_firstLine =
    do assertEqual "Hello World" (firstLine "Hello World")
       assertEqual "Hello World" (firstLine "Hello World\nsecond line")
       assertEqual "Hello World " (firstLine "Hello World \nsecond line")

test_firstParagraph :: IO ()
test_firstParagraph =
    do assertEqual "Hello\nWorld\n" $ firstParagraph "Hello\nWorld"
       assertEqual "Hello\nWorld\n" $ firstParagraph "Hello\nWorld\n"
       assertEqual "Hello\nWorld\n" $ firstParagraph "Hello\nWorld\n\nSecond paragraph.\n"
       assertEqual "Hello\nWorld\n" $ firstParagraph "Hello\nWorld\n\nSecond paragraph."
       assertEqual "Hello\nWorld\n" $ firstParagraph "Hello\nWorld\n\nSecond paragraph.\n\nThird."
       assertEqual "Hello\nWorld\n" $ firstParagraph "Hello\nWorld\n   \nSecond paragraph."

test_escapeXml :: IO ()
test_escapeXml =
    do assertEqual "foobar" (escapeXml "foobar")
       assertEqual "Hallo &lt;Welt&gt;" (escapeXml "Hallo <Welt>")
       assertEqual "Hallo &apos;Welt&gt;" (escapeXml "Hallo 'Welt>")
