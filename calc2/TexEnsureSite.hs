
module TexEnsureSite (ensureSite)
       where

import TexFiles (ensureTexDir, ensurePreamble, ensureDeclarations,
                 ensureTitlePage, ensureToc)


ensureSite :: IO ()
ensureSite
  = do ensureTexDir
       ensurePreamble
       ensureDeclarations
       ensureTitlePage
       ensureToc
