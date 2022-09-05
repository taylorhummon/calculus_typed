
module WebCommon (Html, bs, quote, nl, htmlWrapper)
       where

import DataInfo
import DataPage
import DataLocation

import Info (infoGetPageInfos, fetchPageInfo)
import WebUrls (urlNormalize, urlStyleSheet,
                urlMathJax, urlMathJaxConfig)


type Html = String


bs :: Html
bs = "\\"

quote :: Html
quote = "\""

nl :: Html
nl = "\n"


htmlWrapper :: Info -> Page -> Html -> Html
htmlWrapper i (Page loc title _) body
  = concat [htmlDocType,
            "<html lang=", quote, "en", quote, ">", nl,
            "<head>", nl,
            "<title>", pageTitle i loc title, "</title>", nl,
            htmlNormalize i,
            htmlStyleSheet i,
            htmlMeta,
            htmlFavIcon,
            htmlScriptJax i,
            "</head>", nl,
            "<body>", nl,
            htmlNoScriptWarn,
            "<div class=", quote, "page", quote, ">", nl,
            body,
            "</div>", nl,
            "</body>", nl,
            "</html>", nl]


htmlDocType :: Html
htmlDocType
  = concat ["<!DOCTYPE html>", nl]


htmlNormalize :: Info -> Html
htmlNormalize i
  = concat ["<link rel=", quote, "stylesheet", quote,
            " href=", quote, urlNormalize i, quote,
            " type=", quote, "text/css", quote,
            ">", nl]


htmlStyleSheet :: Info -> Html
htmlStyleSheet i
  = concat ["<link rel=", quote, "stylesheet", quote,
            " href=", quote, urlStyleSheet i, quote,
            " type=", quote, "text/css", quote,
            ">", nl]


htmlMeta :: Html
htmlMeta
  = concat ["<meta charset=", quote, "UTF-8", quote, ">", nl,
            "<meta name=", quote, "author", quote,
            " content=", quote, "Taylor Hummon", quote, ">", nl]

htmlFavIcon :: Html
htmlFavIcon
  = concat ["<link rel=", quote, "apple-touch-icon", quote,
            " sizes=", quote, "180x180", quote,
            " href=", quote, "/apple-touch-icon.png", quote, ">", nl,
            "<link rel=", quote, "icon", quote,
            " type=", quote, "image/png", quote,
            " sizes=", quote, "32x32", quote,
            " href=", quote, "/favicon-32x32.png", quote, ">", nl,
            "<link rel=", quote, "icon", quote,
            " type=", quote, "image/png", quote,
            " sizes=", quote, "16x16", quote,
            " href=", quote, "/favicon-16x16.png", quote, ">", nl,
            "<link rel=", quote, "manifest", quote,
            " href=", quote, "/manifest.json", quote, ">", nl,
            "<link rel=", quote, "mask-icon", quote,
            " href=", quote, "/safari-pinned-tab.svg", quote,
            " color=", quote, "#5bbad5", quote, ">", nl,
            "<meta name=", quote, "theme-color", quote,
            " content=", quote, "#ffffff", quote, ">", nl]


htmlScriptJax :: Info -> Html
htmlScriptJax i
  = concat ["<script type=", quote, "text/javascript", quote,
            " src=", quote,
            urlMathJax i, "?config=", urlMathJaxConfig i,
            quote, ">", nl,
            "</script>", nl]


htmlNoScriptWarn :: Html
htmlNoScriptWarn
  = concat ["<noscript>", nl,
            "<div class=", quote, "jswarning", quote, ">", nl,
            "<a href=", quote, "http://www.mathjax.org/", quote, ">MathJax</a>",
            " requires JavaScript to process the mathematics on this page.", nl,
            "<br />", nl,
            "<strong>", nl,
            "If your browser supports JavaScript, be sure it is enabled.", nl,
            "</strong>", nl,
            "</div>", nl,
            "</noscript>", nl]


pageTitle :: Info -> Location -> String -> String
pageTitle i loc title
  = bookTitle i
    ++ if isTop i loc
       then ""
       else concat [". ", title, "."]


isTop :: Info -> Location -> Bool
isTop i loc
  = case (fetchPageInfo i loc)
    of (PageInfo _ _ PtTop)
         -> True
       _
         -> False


bookTitle :: Info -> String
bookTitle i
  = case infoGetPageInfos i
    of (PageInfo _ title PtTop : _)
         -> title
       _
         -> ""
