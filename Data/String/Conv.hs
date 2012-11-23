{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.String.Conv
    (SLike (..)) where


-------------------------------------------------------------------------------
import           Data.ByteString.Char8      as B
import           Data.ByteString.Lazy.Char8 as LB
import           Data.Text                  as T
import           Data.Text.Encoding         as T
import           Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    as LT
-------------------------------------------------------------------------------


class SLike a b where
    toS :: a -> b


instance SLike String String where toS = id
instance SLike String B.ByteString where toS = B.pack
instance SLike String LB.ByteString where toS = LB.pack
instance SLike String T.Text where toS = T.pack
instance SLike String LT.Text where toS = LT.pack

instance SLike B.ByteString String where toS = B.unpack
instance SLike B.ByteString B.ByteString where toS = id
instance SLike B.ByteString LB.ByteString where toS = LB.fromChunks . return
instance SLike B.ByteString T.Text where toS = T.decodeUtf8
instance SLike B.ByteString LT.Text where toS = toS . LB.fromChunks . return


instance SLike LB.ByteString String where toS = LB.unpack
instance SLike LB.ByteString B.ByteString where toS = B.concat . LB.toChunks
instance SLike LB.ByteString LB.ByteString where toS = id
instance SLike LB.ByteString T.Text where toS = T.decodeUtf8 . toS
instance SLike LB.ByteString LT.Text where toS = LT.decodeUtf8

instance SLike T.Text String where toS = T.unpack
instance SLike T.Text B.ByteString where toS = T.encodeUtf8
instance SLike T.Text LB.ByteString where toS = toS . T.encodeUtf8
instance SLike T.Text LT.Text where toS = LT.fromStrict
instance SLike T.Text T.Text where toS = id

instance SLike LT.Text String where toS = LT.unpack
instance SLike LT.Text T.Text where toS = LT.toStrict
instance SLike LT.Text LT.Text where toS = id
instance SLike LT.Text LB.ByteString where toS = LT.encodeUtf8
instance SLike LT.Text B.ByteString where toS = toS . LT.encodeUtf8







