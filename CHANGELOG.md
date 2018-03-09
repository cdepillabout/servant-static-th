# Changelog

## v0.2.0.1

-   In v0.2.0.0, the new MIME types were not being exported from `Servant.Static.TH`.  This fixes that.

## v0.2.0.0

-   Add
    [support](https://github.com/cdepillabout/servant-static-th/pull/4#pullrequestreview-102307694)
    for ico, eot, ttf, woff, and woff2 file.  Thanks [Alex](https://github.com/delanoe)!

## v0.1.0.6

-   Make it more efficient to embed bytestring filetypes.
-   Make everything other than HTML files be embedded as bytestrings.  Don't
    assume UTF8 encoding.
