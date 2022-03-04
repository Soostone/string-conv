{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString as B
import Data.ByteString.Lazy as LB
import Data.String.Conv
import Data.Text as T
import Data.Text.Lazy as LT
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      " tests"
      [ strictDecoding,
        lenientDecoding
      ]

strictDecoding :: TestTree
strictDecoding =
  testGroup
    "strict decoding (toS method)"
    [ testProperty "converting String to String" $ do
        \s -> s == (toS (toS (s :: String) :: String)),
      testProperty "converting String to strict ByteString" $ do
        \s -> s == (toS (toS (s :: String) :: B.ByteString)),
      testProperty "converting String to lazy ByteString" $ do
        \s -> s == (toS (toS (s :: String) :: LB.ByteString)),
      testProperty "converting String to strict Text" $ do
        \s -> s == (toS (toS (s :: String) :: T.Text)),
      testProperty "converting String to lazy Text" $ do
        \s -> s == (toS (toS (s :: String) :: LT.Text)),
      testProperty "converting strict ByteString to strict ByteString" $ do
        \s -> s == (toS (toS (s :: B.ByteString) :: B.ByteString)),
      testProperty "converting strict ByteString to lazy ByteString" $ do
        \s -> s == (toS (toS (s :: B.ByteString) :: LB.ByteString)),
      testProperty "converting lazy ByteString to lazy ByteString" $ do
        \s -> s == (toS (toS (s :: LB.ByteString) :: LB.ByteString)),
      testProperty "converting strict Text to lazy ByteString" $ do
        \s -> s == (toS (toS (s :: T.Text) :: LB.ByteString)),
      testProperty "converting strict Text to strict ByteString" $ do
        \s -> s == (toS (toS (s :: LB.ByteString) :: B.ByteString)),
      testProperty "converting strict Text to strict Text" $ do
        \s -> s == (toS (toS (s :: T.Text) :: T.Text)),
      testProperty "converting strict Text to lazy Text" $ do
        \s -> s == (toS (toS (s :: T.Text) :: LT.Text)),
      testProperty "converting lazy Text to strict ByteString" $ do
        \s -> s == (toS (toS (s :: LT.Text) :: B.ByteString)),
      testProperty "converting lazy Text to lazy ByteString" $ do
        \s -> s == (toS (toS (s :: LT.Text) :: LB.ByteString)),
      testProperty "converting lazy Text to lazy Text" $ do
        \s -> s == (toS (toS (s :: LT.Text) :: LT.Text))
    ]

lenientDecoding :: TestTree
lenientDecoding =
  testGroup
    "lenient decoding (toSL method)"
    [ testProperty "converting String to String" $ do
        \s -> s == (toSL (toSL (s :: String) :: String)),
      testProperty "converting String to strict ByteString" $ do
        \s -> s == (toSL (toSL (s :: String) :: B.ByteString)),
      testProperty "converting String to lazy ByteString" $ do
        \s -> s == (toSL (toSL (s :: String) :: LB.ByteString)),
      testProperty "converting String to strict Text" $ do
        \s -> s == (toSL (toSL (s :: String) :: T.Text)),
      testProperty "converting String to lazy Text" $ do
        \s -> s == (toSL (toSL (s :: String) :: LT.Text)),
      testProperty "converting strict ByteString to strict ByteString" $ do
        \s -> s == (toSL (toSL (s :: B.ByteString) :: B.ByteString)),
      testProperty "converting strict ByteString to lazy ByteString" $ do
        \s -> s == (toSL (toSL (s :: B.ByteString) :: LB.ByteString)),
      testProperty "converting lazy ByteString to lazy ByteString" $ do
        \s -> s == (toSL (toSL (s :: LB.ByteString) :: LB.ByteString)),
      testProperty "converting strict Text to lazy ByteString" $ do
        \s -> s == (toSL (toSL (s :: T.Text) :: LB.ByteString)),
      testProperty "converting strict Text to strict ByteString" $ do
        \s -> s == (toSL (toSL (s :: LB.ByteString) :: B.ByteString)),
      testProperty "converting strict Text to strict Text" $ do
        \s -> s == (toSL (toSL (s :: T.Text) :: T.Text)),
      testProperty "converting strict Text to lazy Text" $ do
        \s -> s == (toSL (toSL (s :: T.Text) :: LT.Text)),
      testProperty "converting lazy Text to strict ByteString" $ do
        \s -> s == (toSL (toSL (s :: LT.Text) :: B.ByteString)),
      testProperty "converting lazy Text to lazy ByteString" $ do
        \s -> s == (toSL (toSL (s :: LT.Text) :: LB.ByteString)),
      testProperty "converting lazy Text to lazy Text" $ do
        \s -> s == (toSL (toSL (s :: LT.Text) :: LT.Text))
    ]
