{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Servant.Static.TH.Internal.Mime

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports functions and datatypes for using many different mime
types.
-}

module Servant.Static.TH.Internal.Mime where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable (Typeable)
import Language.Haskell.TH
       (Exp(AppE, LitE, VarE), Lit(StringL), Q, Type, appE, stringE, varE)
import Network.HTTP.Media (MediaType, (//))
import Servant.HTML.Blaze (HTML)
import Servant.API (Accept(contentType), MimeRender(mimeRender))
import System.FilePath (takeExtension)
import Text.Blaze.Html (Html, preEscapedToHtml)

import Servant.Static.TH.Internal.Util
       (getExtension, removeLeadingPeriod)

-- | Hold 'Type's and functions work working with a given file type, like
-- @html@ or @js@.
--
-- You can find examples of 'MimeTypeInfo' in the function
-- 'extensionMimeTypeMap'.
data MimeTypeInfo = MimeTypeInfo
  { mimeTypeInfoContentType :: Q Type
    -- ^ A @'Q' 'Type'@ representing a type to use for the content type of a
    -- Servant API.  For instance, HTML files will use something like
    -- @[t|'HTML'|]@, while Javascript files will use something like
    -- @[t|'JS'|]@.
  , mimeTypeInfoRespType :: Q Type
    -- ^ A @'Q' 'Type'@ representing the type to use for the return vale of a
    -- Servant API.  For instance, HTML files will use something like
    -- @[t|'Html'|]@, while JavascriptFiles will use something like
    -- @[t|'ByteString'|]@.
  , mimeTypeInfoToExpression :: ByteString -> Q Exp
    -- ^ A function that turns a 'ByteString' into an 'Exp'.  For an example,
    -- look at 'htmlToExp' and 'byteStringtoExp'.
  }

stringToBs :: String -> ByteString
stringToBs = B8.pack

byteStringToExp :: ByteString -> Q Exp
byteStringToExp byteString = do
  helper <- [| stringToBs |]
  let !chars = B8.unpack byteString
  pure $! AppE (VarE 'pure) $! AppE helper $! LitE $! StringL chars

utf8ByteStringToExp :: ByteString -> Q Exp
utf8ByteStringToExp byteString =
  let stringExp = stringE . unpack $ decodeUtf8With lenientDecode byteString
      packedExp = appE (varE 'pack) stringExp
      byteStringExp = appE (varE 'encodeUtf8) packedExp
  in appE (varE 'pure) byteStringExp

htmlToExp :: ByteString -> Q Exp
htmlToExp byteString =
  let fileContentsString = unpack $ decodeUtf8With lenientDecode byteString
  in [e|pure $ (preEscapedToHtml :: String -> Html) fileContentsString|]

-- | A mapping from an extension like @html@ or @js@ to a 'MimeTypeInfo' for
-- that extension.
extensionMimeTypeMap :: Map String MimeTypeInfo
extensionMimeTypeMap =
  [ ("css",  MimeTypeInfo [t|CSS|]  [t|ByteString|] byteStringToExp)
  , ("gif",  MimeTypeInfo [t|GIF|]  [t|ByteString|] byteStringToExp)
  , ("htm",  MimeTypeInfo [t|HTML|] [t|Html|]             htmlToExp)
  , ("html", MimeTypeInfo [t|HTML|] [t|Html|]             htmlToExp)
  , ("jpeg", MimeTypeInfo [t|JPEG|] [t|ByteString|] byteStringToExp)
  , ("jpg",  MimeTypeInfo [t|JPEG|] [t|ByteString|] byteStringToExp)
  , ("ico",  MimeTypeInfo [t|ICO|]  [t|ByteString|] byteStringToExp)
  , ("js",   MimeTypeInfo [t|JS|]   [t|ByteString|] byteStringToExp)
  , ("png",  MimeTypeInfo [t|PNG|]  [t|ByteString|] byteStringToExp)
  , ("svg",  MimeTypeInfo [t|SVG|]  [t|ByteString|] byteStringToExp)
  , ("txt",  MimeTypeInfo [t|TXT|]  [t|ByteString|] byteStringToExp)
  , ("eot",  MimeTypeInfo [t|EOT|]  [t|ByteString|] byteStringToExp)
  , ("ttf",  MimeTypeInfo [t|TTF|]  [t|ByteString|] byteStringToExp)
  , ("woff", MimeTypeInfo [t|WOFF|] [t|ByteString|] byteStringToExp)
  , ("woff2",MimeTypeInfo [t|WOFF2|][t|ByteString|] byteStringToExp)
  , ("json", MimeTypeInfo [t|JSON|] [t|ByteString|] byteStringToExp)
  , ("xml",  MimeTypeInfo [t|XML|]  [t|ByteString|] byteStringToExp)
  , ("gexf", MimeTypeInfo [t|GEXF|] [t|ByteString|] byteStringToExp)
  , ("map",  MimeTypeInfo [t|JSON|] [t|ByteString|] byteStringToExp)
  , ("wasm", MimeTypeInfo [t|WASM|] [t|ByteString|] byteStringToExp)
  ]

-- | Just like 'extensionToMimeTypeInfo', but throw an error using 'fail' if
-- the extension for the given 'FilePath' is not found.
extensionToMimeTypeInfoEx :: FilePath -> Q MimeTypeInfo
extensionToMimeTypeInfoEx file =
  case extensionToMimeTypeInfo file of
    Just mimeTypeInfo -> pure mimeTypeInfo
    Nothing ->
      let extension = getExtension file
      in fail $
        "Unknown extension type \"" <> extension <> "\".  Please report as bug."

-- | Lookup the 'MimeTypeInfo' for a given 'FilePath' (that has an extension
-- like @.html@ or @.js@).  Returns 'Nothing' if the 'MimeTypeInfo' for the
-- given extension is not found.
extensionToMimeTypeInfo :: FilePath -> Maybe MimeTypeInfo
extensionToMimeTypeInfo file =
  Map.lookup
    (removeLeadingPeriod $ takeExtension file)
    extensionMimeTypeMap

-------------------------
-- Supported MimeTypes --
-------------------------

-- CSS

data CSS deriving Typeable

-- | @text\/css@
instance Accept CSS where
  contentType :: Proxy CSS -> MediaType
  contentType _ = "text" // "css"

instance MimeRender CSS ByteString where
  mimeRender :: Proxy CSS -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- GIF

data GIF deriving Typeable

-- | @image\/gif@
instance Accept GIF where
  contentType :: Proxy GIF -> MediaType
  contentType _ = "image" // "gif"

instance MimeRender GIF ByteString where
  mimeRender :: Proxy GIF -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- JPEG

data JPEG deriving Typeable

-- | @image\/jpeg@
instance Accept JPEG where
  contentType :: Proxy JPEG -> MediaType
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender :: Proxy JPEG -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict



-- ICO

-- | @since 0.2.0.0
data ICO deriving Typeable

-- | @icon\/ico@
instance Accept ICO where
  contentType :: Proxy ICO -> MediaType
  contentType _ = "image" // "x-icon"

instance MimeRender ICO ByteString where
  mimeRender :: Proxy ICO -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- JS

data JS deriving Typeable

-- | @application\/javascript@
instance Accept JS where
  contentType :: Proxy JS -> MediaType
  contentType _ = "application" // "javascript"

instance MimeRender JS ByteString where
  mimeRender :: Proxy JS -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- PNG

data PNG deriving Typeable

-- | @image\/png@
instance Accept PNG where
  contentType :: Proxy PNG -> MediaType
  contentType _ = "image" // "png"

instance MimeRender PNG ByteString where
  mimeRender :: Proxy PNG -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- SVG

data SVG deriving Typeable

-- | @image\/svg@
instance Accept SVG where
  contentType :: Proxy SVG -> MediaType
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG ByteString where
  mimeRender :: Proxy SVG -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- TXT

data TXT deriving Typeable

-- | @text\/plain@
instance Accept TXT where
  contentType :: Proxy TXT -> MediaType
  contentType _ = "text" // "plain"

instance MimeRender TXT ByteString where
  mimeRender :: Proxy TXT -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- EOT

-- | @since 0.2.0.0
data EOT deriving Typeable

-- | @fonts\/eot@
instance Accept EOT where
  contentType :: Proxy EOT -> MediaType
  contentType _ = "application" // "vnd.ms-fontobject"

instance MimeRender EOT ByteString where
  mimeRender :: Proxy EOT -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- TTF

-- | @since 0.2.0.0
data TTF deriving Typeable

-- | @fonts\/ttf@
instance Accept TTF where
  contentType :: Proxy TTF -> MediaType
  contentType _ = "application" // "x-font-truetype"

instance MimeRender TTF ByteString where
  mimeRender :: Proxy TTF -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- WOFF

-- | @since 0.2.0.0
data WOFF deriving Typeable

-- | @fonts\/woff@
instance Accept WOFF where
  contentType :: Proxy WOFF -> MediaType
  contentType _ = "font" // "woff"

instance MimeRender WOFF ByteString where
  mimeRender :: Proxy WOFF -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- WOFF2

-- | @since 0.2.0.0
data WOFF2 deriving Typeable

-- | @fonts\/woff2@
instance Accept WOFF2 where
  contentType :: Proxy WOFF2 -> MediaType
  contentType _ = "font" // "woff2"

instance MimeRender WOFF2 ByteString where
  mimeRender :: Proxy WOFF2 -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict


-- | JSON file
data JSON deriving Typeable

-- | @application\/json@
instance Accept JSON where
  contentType :: Proxy JSON -> MediaType
  contentType _ = "application" // "json"

instance MimeRender JSON ByteString where
  mimeRender :: Proxy JSON -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict


-- | XML file
data XML deriving Typeable

-- | @application\/xml@
instance Accept XML where
  contentType :: Proxy XML -> MediaType
  contentType _ = "application" // "xml"

instance MimeRender XML ByteString where
  mimeRender :: Proxy XML -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict


-- | GEXF file (xml for graph application)
data GEXF deriving Typeable

-- | @application\/gexf@
instance Accept GEXF where
  contentType :: Proxy GEXF -> MediaType
  contentType _ = "application" // "gexf"

instance MimeRender GEXF ByteString where
  mimeRender :: Proxy GEXF -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict


-- | WASM file (WebAssembly bytecode)
data WASM deriving Typeable

-- | @application\/wasm@
instance Accept WASM where
  contentType :: Proxy WASM -> MediaType
  contentType _ = "application" // "wasm"

instance MimeRender WASM ByteString where
  mimeRender :: Proxy WASM -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict
