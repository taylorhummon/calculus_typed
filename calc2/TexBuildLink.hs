
module TexBuildLink (buildLink, buildAnchor)
       where

import DataInfo
import DataLink
import DataLocation

import TexBuildPhrase (buildPhrase)
import TexCommon (Tex, bs)


buildLink :: Info -> Link -> Tex
buildLink _ (LkExternal url phrase)
  = concat [bs, "href", "{", url, "}",
            "{", buildPhrase phrase, "}"]
buildLink _ (LkTarget _)
  = error "Encountered LkTarget at build time."
buildLink _ (LkInternal loc phrase)
  = concat [bs, "hyperref", "[", buildAnchor loc, "]",
            "{", buildPhrase phrase, "}"]
buildLink _ (LkString loc str)
  = concat [bs, "hyperref", "[", buildAnchor loc, "]",
            "{", str, "}"]
buildLink _ (LkFail phrase)
  = buildPhrase phrase



buildAnchor :: Location -> String
buildAnchor loc
  = "sec:" ++ buildAnchor' loc

buildAnchor' :: Location -> String
buildAnchor' []
  = []
buildAnchor' (locId : [])
  = locId
buildAnchor' (locId : rest)
  = locId ++ "_" ++ buildAnchor' rest


