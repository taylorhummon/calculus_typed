
module WebUrls
       (Url
       ,domain
       ,urlLanding
       ,urlStyleSheet
       ,urlNormalize
       ,urlMathJax
       ,urlMathJaxConfig
       ,urlLink
       ,urlImg
       )
       where

import DataInfo
import DataLocation

import Info (infoGetRun)
import Data.List (intercalate)


type Url = String


domain :: Info -> Url
domain info
  = case infoGetRun info
    of RunTex
         -> ""
       RunWeb
         -> "/ct/"
       RunWebRebuild
         -> "/ct/"
       RunWebPublish
         -> "https://calculustyped.taylorhummon.com/book/"


urlLanding :: Info -> Maybe Url
urlLanding info
  = case infoGetRun info
    of RunTex
         -> Nothing
       RunWeb
         -> Nothing
       RunWebRebuild
         -> Nothing
       RunWebPublish
         -> Just "https://calculustyped.taylorhummon.com/"


urlStyleSheet :: Info -> Url
urlStyleSheet i
  = domain i ++ "ct.css"


urlNormalize :: Info -> Url
urlNormalize i
  = domain i ++ "normalize.css"


urlMathJax :: Info -> Url
urlMathJax i
  = domain i ++ "mathjax/MathJax.js"


urlMathJaxConfig :: Info -> Url
urlMathJaxConfig i
  = domain i ++ "jaxconfig.js"


urlLink :: Info -> Location -> Url
urlLink i loc
  = domain i ++ urlFromLoc loc


-- discard top pageID (calculus) when creating URL
urlFromLoc :: Location -> Url
urlFromLoc []
  = []
urlFromLoc (_ : [])
  = []
urlFromLoc (_ : rest)
  = intercalate "/" rest ++ "/"


urlImg :: Info -> Location -> String -> Url
urlImg i loc file
  = urlLink i loc ++ file ++ ".svg"
