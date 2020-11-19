
-- See TexFiles, WebFiles, and SystemCalls for other file paths.

module Files
       (Path
       ,ImageName(..)
       ,topDir
       ,ensureDir
       ,ensureFileCopy
       ,ensureLink
       ,ensureDirCopy
       ,isOutDated
       ,isPageOutDated
       ,writePage
       ,isImageOutDated
       ,writeImage
       ,readFileByLoc
       ,readContentsFile
       )
       where

import DataLocation

import Except (Excepted, Exception(..), throw)
import Message (exitFail,
                messageUpdated, messageMissing,
                messagePageUpdated, messagePageMissing,
                messageImageUpdated, messageImageMissing)
import SystemCalls (topDir, copyDir)
import System.Directory (doesFileExist, doesDirectoryExist,
                         createDirectoryIfMissing,
                         getModificationTime, copyFile)
import System.Posix (createSymbolicLink)


type Path = String

data ImageName
  = ImageName String String String



{- *** Paths *** -}

bookDir :: Path
bookDir
  = topDir ++ "book/"


contentsFile :: Path
contentsFile
  = bookDir ++ "contents.txt"


pathPgFileIn :: Location -> Path
pathPgFileIn loc
  = bookDir ++ pathFromLoc loc ++ ".calc"


pathImgDirIn :: Location -> Path
pathImgDirIn loc
  = bookDir ++ pathFromLoc loc ++ ".images/"


pathImgFileIn :: Location -> ImageName -> Path
pathImgFileIn loc (ImageName prefix name extension)
  = pathImgDirIn loc ++ prefix ++ "_" ++ name ++ "." ++ extension


pathFromLoc :: Location -> Path
pathFromLoc (pageId : [])
  = pageId
pathFromLoc (pageId : rest)
  = pageId ++ ".material/" ++ pathFromLoc rest
pathFromLoc []
  = []


ensureDir :: Path -> IO ()
ensureDir path
  = do b <- doesDirectoryExist path
       if b
         then return ()
         else do createDirectoryIfMissing True path


isOutDated :: Path -> Path -> IO Bool
isOutDated inFile outFile
  = do inExists <- doesFileExist inFile
       if inExists
         then do outExists <- doesFileExist outFile
                 if outExists
                   then do inTime <- getModificationTime inFile
                           outTime <- getModificationTime outFile
                           return (inTime > outTime)
                   else return True
         else return True


ensureFileCopy :: String -> Path -> Path -> IO ()
ensureFileCopy str source target
  = do b1 <- doesFileExist source
       if not b1
         then do messageMissing str
         else do b2 <- isOutDated source target
                 if b2
                   then do copyFile source target
                           messageUpdated str
                   else do return ()


ensureLink :: String -> Path -> Path -> IO ()
ensureLink str source target
  = do b1 <- doesDirectoryExist source
       if not b1
         then messageMissing str
         else do b2 <- doesDirectoryExist target
                 if b2
                   then return ()
                   else do createSymbolicLink source target
                           messageUpdated str


ensureDirCopy :: String -> Path -> Path -> IO ()
ensureDirCopy str source target
  = do b1 <- doesDirectoryExist source
       if not b1
         then messageMissing str
         else do b2 <- doesDirectoryExist target
                 if b2
                   then return ()
                   else do success <- copyDir source target
                           if success
                             then messageUpdated str
                             else messageMissing str



{- *** Page File Manipulation *** -}

pageExists :: Location -> IO Bool
pageExists loc
  = doesFileExist (pathPgFileIn loc)


isPageOutDated :: Location -> Path -> IO Bool
isPageOutDated loc outFile
  = isOutDated (pathPgFileIn loc) outFile


writePage :: Location -> Path -> String -> IO Bool
writePage loc path out
  = do b <- pageExists loc
       if b
         then do writeFile path out
                 messagePageUpdated loc
                 return False
         else do messagePageMissing loc
                 return True



{- *** Image File Manipulation *** -}

imageExists :: Location -> ImageName -> IO Bool
imageExists loc image
  = doesFileExist (pathImgFileIn loc image)


isImageOutDated :: Location -> Path -> ImageName -> IO Bool
isImageOutDated loc outpath image
  = isOutDated (pathImgFileIn loc image) outpath


writeImage :: Location -> Path -> ImageName -> IO Bool
writeImage loc outpath image
  = do b <- imageExists loc image
       if b
         then do copyFile (pathImgFileIn loc image) outpath
                 messageImageUpdated loc imageString
                 return True
         else do messageImageMissing loc imageString
                 return False
    where ImageName _ imageString _ = image


{- *** Read File by Location *** -} 

readFileByLoc :: Location -> IO (Excepted String)
readFileByLoc loc
  = do let path = pathPgFileIn loc
       b <- doesFileExist path
       if b
         then do file <- readFile path
                 return (return file)
         else do return (throw (ExceptMessage "Source file does not exist."))
                               



{- *** Read From Contents File *** -}

readContentsFile :: IO (String)
readContentsFile
  = do exists <- doesFileExist contentsFile
       if exists
         then readFile contentsFile
         else exitFail "Could not find contents file."

