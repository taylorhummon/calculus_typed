
module TexBuildMath (buildMath, buildMath')
       where

import Compose
import DataMath

import TexCommon (Tex, nl)
import TexBuildPhrase (buildPhrase)


buildMath :: Math -> Tex
buildMath math
  = buildMath' True True math


buildMath' :: Bool -> Bool -> Math -> Tex
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


buildMathElem :: MathElem -> Tex
buildMathElem (MeVariable c)
  = [c]
buildMathElem (MeNumber str)
  = str
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
buildMathElem (MeCommand "\\" [])
  = buildCommand "\\" ++ nl
buildMathElem (MeCommand str maths)
  = buildCommand str ++ concatMap buildBraceArgument maths
buildMathElem (MeCommandOpt str Nothing maths)
  = buildCommand str ++ concatMap buildBraceArgument maths
buildMathElem (MeCommandOpt str (Just math) maths)
  = buildCommand str
    ++ "[" ++ buildMath math ++ "]"
    ++ concatMap buildBraceArgument maths


buildCommand :: String -> Tex
buildCommand str
  = "\\" ++ str


buildBraceArgument :: Math -> Tex
buildBraceArgument math
  = encloseBraces (buildMath math)


encloseBraces :: Tex -> Tex
encloseBraces str
  = "{" ++ str ++ "}"

