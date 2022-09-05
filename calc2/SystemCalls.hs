
module SystemCalls (topDir, bulldozeTex, bulldozeWeb,
                    bulldozePublish, copyDir)
       where

import System.Process (system)
import System.Exit (ExitCode(..))


topDir :: String
topDir
  = "/Users/taylor/else/projects/calculus_typed/"   -- TODO: move this to a config file


removeDir :: String -> String
removeDir str
  = "rm -fr " ++ topDir ++ str


bulldozeTex :: IO Bool
bulldozeTex
  = do exitCode <- system (removeDir "tex")
       return (exitCode == ExitSuccess)


bulldozeWeb :: IO Bool
bulldozeWeb
  = do exitCode <- system (removeDir "web")
       return (exitCode == ExitSuccess)


bulldozePublish :: IO Bool
bulldozePublish
  = do exitCode <- system (removeDir "publish")
       return (exitCode == ExitSuccess)


copyDir ::  String -> String -> IO Bool
copyDir source target
  = do exitCode <- system ("cp -r " ++ source ++ " " ++ target)
       return (exitCode == ExitSuccess)
