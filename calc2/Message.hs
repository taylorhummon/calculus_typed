
module Message (exitSuccess,
                exitFail,
                exitParsing,
                messageException,
                messagePageUpdated,
                messagePageMissing,
                messageImageUpdated,
                messageImageMissing,
                messageUpdated,
                messageMissing,
                messageBadLink,
               )
       where

import DataLocation

import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))



showLoc :: Location -> String
showLoc (pageId : [])
  = pageId
showLoc (pageId : rest)
  = pageId ++ "/" ++ showLoc rest
showLoc []
  = []


exitSuccess :: String -> IO a
exitSuccess str
  = do hPutStrLn stderr str
       exitWith ExitSuccess


exitFail :: String -> IO a
exitFail str
  = do hPutStrLn stderr str
       exitWith (ExitFailure 1)


exitParsing :: String -> IO a
exitParsing str
  = do hPutStrLn stderr "Could not parse contents file."
       exitFail str


messageException :: Location -> String -> IO ()
messageException loc str
  = do putStrLn ("page: " ++ showLoc loc)
       putStrLn ("  " ++ str)


messagePageUpdated :: Location -> IO ()
messagePageUpdated loc
  = putStrLn (showLoc loc ++ ". " ++ "page updated.")


messagePageMissing :: Location -> IO ()
messagePageMissing loc
  = putStrLn (showLoc loc ++ "/" ++ ". " ++ "PAGE MISSING.")


messageImageUpdated :: Location -> String -> IO ()
messageImageUpdated loc image
  = putStrLn (showLoc loc ++ "/" ++ image ++ ". " ++ "image updated.")


messageImageMissing :: Location -> String -> IO ()
messageImageMissing loc image
  = putStrLn (showLoc loc ++ "/" ++ image ++ ". " ++ "IMAGE MISSING.")


messageUpdated :: String -> IO ()
messageUpdated str
  = putStrLn (str ++ ". " ++ "updated.")


messageMissing :: String -> IO ()
messageMissing str
  = putStrLn (str ++ ". " ++ "MISSING.")


messageBadLink :: Location -> Location -> IO ()
messageBadLink loc loc'
  = putStrLn (concat [showLoc loc, ". ",
                      "COULD NOT CREATE LINK: ",
                      showLoc loc'])


