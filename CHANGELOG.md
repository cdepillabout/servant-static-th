# Changelog

## v0.2.2.1

-   Update haddocks with a note to use `extra-source-files` in your `.cabal`
    file.  Thanks [fkellner](https://github.com/fkellner)! [#8](https://github.com/cdepillabout/servant-static-th/pull/8)

## v0.2.2.0

-   This adds MIME types for json, xml, gex.  Thanks [delanoe](https://github.com/delanoe)!  [#5](https://github.com/cdepillabout/servant-static-th/pull/5)

## v0.2.1.0

-   This was mistakenly released to Hackage without exposing the constructors added in [#5](https://github.com/cdepillabout/servant-static-th/pull/5)).  It has been blacklisted on Hackage.

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
