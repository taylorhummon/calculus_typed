
module WebBuildMath (buildMath, buildMath')
       where

import Compose
import DataMath

import WebCommon (Html)
import WebBuildPhrase (buildPhrase)


buildMath :: Math -> Html
buildMath math
  = buildMath' True True math


buildMath' :: Bool -> Bool -> Math -> Html
buildMath' removeSpaceL removeSpaceR math
  = math
    $> cleanupL removeSpaceL
    $> cleanupR removeSpaceR
    $> concatMap buildMathElem

cleanupL :: Bool -> Math -> Math
cleanupL True (MeSpace : rest)
  = cleanupL True rest
cleanupL _ math
  = math

cleanupR :: Bool -> Math -> Math
cleanupR bool math
  = math
    $> reverse
    $> cleanupL bool
    $> reverse


buildMathElem :: MathElem -> Html
buildMathElem (MeVariable c)
  = [c]
buildMathElem (MeNumber str)
  = str
buildMathElem (MeSymbol '&')
  = "&amp;"
buildMathElem (MeSymbol '"')
  = "&quot;"
buildMathElem (MeSymbol c)
  = [c]
buildMathElem MeSpace
  = " "
buildMathElem (MeText text)
  = buildCommand "text" ++ encloseBraces (buildPhrase text)
buildMathElem (MeUnsafe str)
  = str
buildMathElem (MeSubScript math)
  = "_" ++ buildBraceArgument math
buildMathElem (MeSuperScript math)
  = "^" ++ buildBraceArgument math
buildMathElem (MeCommand str maths)
  = buildCommand str ++ concatMap buildBraceArgument maths
buildMathElem (MeCommandOpt str Nothing maths)
  = buildCommand str ++ concatMap buildBraceArgument maths
buildMathElem (MeCommandOpt str (Just math) maths)
  = buildCommand str
    ++ "[" ++ buildMath math ++ "]"
    ++ concatMap buildBraceArgument maths


buildCommand :: String -> Html
buildCommand str
  = "\\" ++ str


buildBraceArgument :: Math -> Html
buildBraceArgument math
  = encloseBraces (buildMath math)


encloseBraces :: Html -> Html
encloseBraces str
  = "{" ++ str ++ "}"

