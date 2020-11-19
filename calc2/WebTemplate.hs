
module WebTemplate
       (ensureTemplate)
       where

import DataInfo

import Message (messageUpdated, messageMissing)
import WebUrls (domain)

import qualified Files as F (Path, isOutDated)
import System.Directory (doesFileExist)


ensureTemplate :: Info -> String -> F.Path -> F.Path -> IO ()
ensureTemplate i str source target
  = do b1 <- doesFileExist source
       if not b1
         then do messageMissing str
         else do b2 <- F.isOutDated source target
                 if b2
                   then do contents <- readFile source
                           writeFile target (process i contents)
                           messageUpdated str
                   else do return ()


process :: Info -> String -> String
process _ []
  = []
process i str @ (c : cs)
  = case beginsWith "{DOMAIN}" str
    of Nothing
         -> c : process i cs
       Just rest
         -> domain i ++ process i rest


beginsWith :: String -> String -> Maybe String
beginsWith [] str
  = Just str
beginsWith (_ : _) []
  = Nothing
beginsWith (c : cs) (c' : cs')
  | c == c'   = beginsWith cs cs'
  | otherwise = Nothing

  
