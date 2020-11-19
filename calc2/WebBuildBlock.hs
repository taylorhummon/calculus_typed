
module WebBuildBlock (buildTag, buildDiv)
       where

import Compose
import DataInfo
import DataBlock

import Data.List (intersperse)
import WebCommon (Html, quote, nl)


buildTag :: Info -> String -> Block a -> Html -> Html
buildTag _ str (Block _ (b1, b2) _) html
  = concat ["<", str, classStr Nothing b1 b2, ">", nl,
            html,
            "</", str, ">", nl]


buildDiv :: Info -> String -> Block a -> Html -> Html
buildDiv _ str (Block _ (b1, b2) _) html
  = concat ["<div", classStr (Just str) b1 b2, ">", nl,
            html,
            "</div>", nl]


classStr :: Maybe String -> Bool -> Bool -> String
classStr Nothing False False
  = ""
classStr ms b1 b2
  = concat [" class=", quote,
            classStr' [ms, maybeTop b1, maybeBottom b2],
            quote]


maybeTop :: Bool -> Maybe String
maybeTop False
  = Nothing
maybeTop True
  = Just "ct"
    
maybeBottom :: Bool -> Maybe String
maybeBottom False
  = Nothing
maybeBottom True
  = Just "cb"


classStr' :: [Maybe String] -> String
classStr'
  = keepJusts
    .> intersperse " "
    .> concat


keepJusts :: [Maybe String] -> [String]
keepJusts []
  = []
keepJusts (Nothing : rest)
  = keepJusts rest
keepJusts (Just s : rest)
  = s : keepJusts rest

