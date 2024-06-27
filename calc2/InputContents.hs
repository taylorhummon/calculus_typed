
module InputContents (inputContents)
       where

import Compose
import DataContents

import Message (exitParsing)
import Files (readContentsFile)


type Line = (Int, String)


inputContents :: IO Contents
inputContents
  = do str <- readContentsFile
       contents <- getContentsList 0 (numberedLines str)
       assertUnique contents


getContentsList :: Int -> [Line] -> IO [Contents]
getContentsList _ []
  = return []
getContentsList indent ((ln, str) : rest)
  = case grabLine indent str
    of Just cl
         -> do let (inside, after) = breakLines indent [] rest
               subs <- getContentsList (indent + 2) inside
               let contents = Contents cl subs
               contentsRest <- getContentsList indent after
               return (contents : contentsRest)
       _
         -> exitParsing ("Line: " ++ show ln)


grabLine :: Int -> String -> Maybe ContentsLine
grabLine 0 (' ' : _)
  = Nothing
grabLine 0 str
  = parseLine str
grabLine indent (' ' : rest)
  = grabLine (indent - 1) rest
grabLine _ _
  = Nothing


breakLines :: Int -> [Line] -> [Line] -> ([Line], [Line])
breakLines _ acc []
  = (reverse acc, [])
breakLines indent acc ((ln, str) : pairs)
  = case grabLine indent str
    of Just _
         -> (reverse acc, ((ln, str) : pairs))
       Nothing
         -> breakLines indent ((ln, str) : acc) pairs


parseLine :: String -> Maybe ContentsLine
parseLine str
  = do (pageId, rest) <- breakPageId str
       title <- getTitle rest
       return (ContentsLine pageId title)


breakPageId :: String -> Maybe (String, String)
breakPageId str
  = breakPageId' "" str


breakPageId' :: String -> String -> Maybe (String, String)
breakPageId' _ ""
  = Nothing
breakPageId' acc (rest@(' ' : _))
  = Just (reverse acc, rest)
breakPageId' acc (c : cs)
  = breakPageId' (c : acc) cs


getTitle :: String -> Maybe String
getTitle
  = dropWs .> reverse .> dropWs .> reverse .> getTitle'


getTitle' :: String -> Maybe String
getTitle' []
  = Nothing
getTitle' "\""
  = Nothing
getTitle' str
  = if (head str == '"' && last str == '"')
    then let str' = init (tail str)
         in Just str'
    else Nothing


dropWs :: String -> String
dropWs (' ' : cs) = dropWs cs
dropWs str        = str


numberedLines :: String -> [Line]
numberedLines
  = lines
    .> zip [1..]
    .> map (fmap removeComments)
    .> filter (snd .> emptyLine .> not)


removeComments :: String -> String
removeComments []
  = []
removeComments ('%' : _)
  = []
removeComments (c : cs)
  = (c : removeComments cs)


emptyLine :: String -> Bool
emptyLine []
  = True
emptyLine (' ' : rest)
  = emptyLine rest
emptyLine _
  = False


assertUnique :: [Contents] -> IO Contents
assertUnique [the]
  = return the
assertUnique _
  = exitParsing ("Must have a unique top-level location.")
