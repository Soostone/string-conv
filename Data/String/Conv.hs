{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.String.Conv
    (StringConv (..), convS) where


------------------------------------------------------------------------------
import           Data.ByteString.Char8      as B
import           Data.ByteString.Lazy.Char8 as LB
import           Data.Text                  as T
import           Data.Text.Encoding         as T
import           Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    as LT
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A type class to standardize string conversions.  With this type class you
-- only need to remember one function for converting between any two string
-- variants.  This package includes support for String, ByteString, and Text
-- as well as the Lazy and Strict variants where necessary.
--
-- StringConv's `toS` function is most useful when you have a fully defined
-- string conversion with a fixed (non-polymorphic) input and output type.  Of
-- course you can still use it when you don't have a fixed type.  In that case
-- you might need to specify a type class constraint such as @StringConv
-- s String@.
class StringConv a b where
    -- | Universal string conversion function.
    toS :: a -> b


instance StringConv String String where toS = id
instance StringConv String B.ByteString where toS = B.pack
instance StringConv String LB.ByteString where toS = LB.pack
instance StringConv String T.Text where toS = T.pack
instance StringConv String LT.Text where toS = LT.pack

instance StringConv B.ByteString String where toS = B.unpack
instance StringConv B.ByteString B.ByteString where toS = id
instance StringConv B.ByteString LB.ByteString where toS = LB.fromChunks . return
instance StringConv B.ByteString T.Text where toS = T.decodeUtf8
instance StringConv B.ByteString LT.Text where toS = toS . LB.fromChunks . return

instance StringConv LB.ByteString String where toS = LB.unpack
instance StringConv LB.ByteString B.ByteString where toS = B.concat . LB.toChunks
instance StringConv LB.ByteString LB.ByteString where toS = id
instance StringConv LB.ByteString T.Text where toS = T.decodeUtf8 . toS
instance StringConv LB.ByteString LT.Text where toS = LT.decodeUtf8

instance StringConv T.Text String where toS = T.unpack
instance StringConv T.Text B.ByteString where toS = T.encodeUtf8
instance StringConv T.Text LB.ByteString where toS = toS . T.encodeUtf8
instance StringConv T.Text LT.Text where toS = LT.fromStrict
instance StringConv T.Text T.Text where toS = id

instance StringConv LT.Text String where toS = LT.unpack
instance StringConv LT.Text T.Text where toS = LT.toStrict
instance StringConv LT.Text LT.Text where toS = id
instance StringConv LT.Text LB.ByteString where toS = LT.encodeUtf8
instance StringConv LT.Text B.ByteString where toS = toS . LT.encodeUtf8


------------------------------------------------------------------------------
-- | A lens for toS.
convS :: (StringConv a b, StringConv b a, Functor f) => (b -> f b) -> a -> f a
convS f a = fmap toS (f (toS a))

