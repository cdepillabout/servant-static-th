{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}

module Servant.Static.TH.Internal.CompressedData where

import Servant.API.ResponseHeaders ( Headers
                                   , addHeader
                                   , noHeader )
import Servant.API.Header ( Header )
import Language.Haskell.TH.Syntax ( Lift )

-- | Supported compression encodings
--   aligned with https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
--   There can be more items in the future, like 'br' or 'zstd'.
data CompressionType = Gzip
                     | Brotli
                     | Identity
                     deriving (Lift)

instance Show CompressionType where
  show x = case x of
    Gzip     -> "gzip"
    Brotli   -> "br"
    Identity -> "identity"

-- | The type to indicate that the response supports content-encoding
type EncodingAwareResponse response = Headers '[ Header "Content-Encoding" String ] response

-- | A utility function to conveniently add a correct 'Content-Encoding' header
addCompressionHeader :: CompressionType
                     -> response
                     -> EncodingAwareResponse response
addCompressionHeader ct = case ct of
  Identity -> noHeader
  anyOther -> addHeader $ show anyOther

-- | returns a corresponding 'CompressionType' from the provided extension suffix
fromExt :: String -> CompressionType
fromExt ct = case ct of
  "gz" -> Gzip
  "br" -> Brotli
  _ -> Identity
