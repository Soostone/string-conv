{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.String.Conv
  ( StringConv (..)
  , toS
  , toSL
  , convS
  , convSL
  , Leniency (..)
  ) where

------------------------------------------------------------------------------
import           Data.ByteString            as B
import           Data.ByteString.Lazy       as LB
import           Data.Text                  as T
import           Data.Text.Encoding         as T
import           Data.Text.Encoding.Error   as T
import           Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    as LT
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Data type representing the two leniency modes defining how decoding
-- failure is handled.
data Leniency = Lenient | Strict
  deriving (Eq,Show,Read,Ord,Enum,Bounded)


------------------------------------------------------------------------------
-- | A type class to standardize string conversions.  With this type class you
-- only need to remember one function for converting between any two string
-- variants.  This package includes support for String, ByteString, and Text
-- as well as the Lazy and Strict variants where necessary.
--
-- This type class lets you control how conversion should behave when failure
-- is possible.  Strict mode will cause an exception to be thrown when
-- decoding fails.  Lenient mode will attempt to recover, inserting a
-- replacement character for invalid bytes.
--
-- StringConv's `toS` function is most useful when you have a fully defined
-- string conversion with a fixed (non-polymorphic) input and output type.  Of
-- course you can still use it when you don't have a fixed type.  In that case
-- you might need to specify a type class constraint such as @StringConv
-- s String@.
class StringConv a b where
    strConv :: Leniency -> a -> b


------------------------------------------------------------------------------
-- | Universal string conversion function for strict decoding.
toS :: StringConv a b => a -> b
toS = strConv Strict


------------------------------------------------------------------------------
-- | Universal string conversion function for lenient decoding.
toSL :: StringConv a b => a -> b
toSL = strConv Lenient


instance StringConv String String where strConv _ = id
instance StringConv String B.ByteString where strConv _ = T.encodeUtf8 . T.pack
instance StringConv String LB.ByteString where strConv _ = LT.encodeUtf8 . LT.pack
instance StringConv String T.Text where strConv _ = T.pack
instance StringConv String LT.Text where strConv _ = LT.pack

instance StringConv B.ByteString String where strConv l = T.unpack . decodeUtf8T l
instance StringConv B.ByteString B.ByteString where strConv _ = id
instance StringConv B.ByteString LB.ByteString where strConv _ = LB.fromChunks . return
instance StringConv B.ByteString T.Text where strConv = decodeUtf8T
instance StringConv B.ByteString LT.Text where strConv l = strConv l . LB.fromChunks . return

instance StringConv LB.ByteString String where strConv l = LT.unpack . decodeUtf8LT l
instance StringConv LB.ByteString B.ByteString where strConv _ = B.concat . LB.toChunks
instance StringConv LB.ByteString LB.ByteString where strConv _ = id
instance StringConv LB.ByteString T.Text where strConv l = decodeUtf8T l . strConv l
instance StringConv LB.ByteString LT.Text where strConv = decodeUtf8LT

instance StringConv T.Text String where strConv _ = T.unpack
instance StringConv T.Text B.ByteString where strConv _ = T.encodeUtf8
instance StringConv T.Text LB.ByteString where strConv l = strConv l . T.encodeUtf8
instance StringConv T.Text LT.Text where strConv _ = LT.fromStrict
instance StringConv T.Text T.Text where strConv _ = id

instance StringConv LT.Text String where strConv _ = LT.unpack
instance StringConv LT.Text T.Text where strConv _ = LT.toStrict
instance StringConv LT.Text LT.Text where strConv _ = id
instance StringConv LT.Text LB.ByteString where strConv _ = LT.encodeUtf8
instance StringConv LT.Text B.ByteString where strConv l = strConv l . LT.encodeUtf8


------------------------------------------------------------------------------
-- | Convenience helper for dispatching based on leniency.
decodeUtf8T :: Leniency -> B.ByteString -> T.Text
decodeUtf8T Lenient = T.decodeUtf8With T.lenientDecode
decodeUtf8T Strict = T.decodeUtf8With T.strictDecode


------------------------------------------------------------------------------
-- | Convenience helper for dispatching based on leniency.
decodeUtf8LT :: Leniency -> LB.ByteString -> LT.Text
decodeUtf8LT Lenient = LT.decodeUtf8With T.lenientDecode
decodeUtf8LT Strict = LT.decodeUtf8With T.strictDecode


------------------------------------------------------------------------------
-- | A lens for 'toS' to make it slightly more convenient in some scenarios.
convS :: (StringConv a b, StringConv b a, Functor f) => (b -> f b) -> a -> f a
convS f a = fmap toS (f (toS a))


------------------------------------------------------------------------------
-- | A lens for 'toSL' to make it slightly more convenient in some scenarios.
convSL :: (StringConv a b, StringConv b a, Functor f) => (b -> f b) -> a -> f a
convSL f a = fmap toSL (f (toSL a))

