module Rules.Bindist where

import Expression
import GHC
import Oracles.Setting
import Settings
import System.Directory (getCurrentDirectory)
import Target
import Utilities

bindistRules :: Rules ()
bindistRules = do
    phony "binary-dist" $ do
      -- This is kind of incorrect.  We should not "need" a phony rule.
      -- Instead we should *need* the libraries and binaries we want to
      -- put into the binary distribution.  For now we will just *need*
      -- stage2 and package up bin and lib.
      need ["stage2", "docs"]
      root <- buildRoot
      version <- setting ProjectVersion
      cwd <- liftIO getCurrentDirectory
      let baseDir = root -/- stageString Stage1
      targetPlatform <- setting TargetPlatformFull

      -- prepare binary distribution configure script
      copyFile (cwd -/- "aclocal.m4") (cwd -/- "distrib" -/- "aclocal.m4")
      buildWithCmdOptions [] $
        target (vanillaContext Stage1 ghc) (Autoreconf $ cwd -/- "distrib") [] []

      let ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform
          bindistFilesDir  = root -/- ghcVersionPretty
      createDirectory bindistFilesDir

      liftIO $ writeFile (bindistFilesDir -/- "Makefile") bindistMakefile
      -- copy/move config.sub, config.guess, install-sh, etc files
      -- from the source of the tree to the bindist dir
      moveFile (cwd -/- "distrib" -/- "configure") (bindistFilesDir -/- "configure")
      copyFile (cwd -/- "install-sh") (bindistFilesDir -/- "install-sh")
      copyFile (cwd -/- "config.sub") (bindistFilesDir -/- "config.sub")
      copyFile (cwd -/- "config.guess") (bindistFilesDir -/- "config.guess")
      copyFile (cwd -/- "settings.in") (bindistFilesDir -/- "settings.in")
      copyFile (cwd -/- "mk" -/- "config.mk.in") (bindistFilesDir -/- "mk" -/- "config.mk.in")
      copyFile (cwd -/- "mk" -/- "install.mk.in") (bindistFilesDir -/- "mk" -/- "install.mk.in")
      copyDirectory (baseDir -/- "bin") bindistFilesDir
      copyDirectory (baseDir -/- "lib") bindistFilesDir
      copyDirectory (takeDirectory baseDir -/- "docs") bindistFilesDir

      command [Cwd root] "tar"
        [ "-c", "--xz", "-f"
        , ghcVersionPretty++".tar.xz"
        , ghcVersionPretty
        ]

bindistMakefile :: String
bindistMakefile = unlines
  [ "MAKEFLAGS += --no-builtin-rules"
  , ".SUFFIXES:"
  , ""
  , "include mk/install.mk"
  , ""
  , ".PHONY: default"
  , "default:"
  , "\t@echo 'Run \"make install\" to install'"
  , "\t@false"
  , ""
  , ".PHONY: install"
  , "install:"
  , "\tmkdir -p $(prefix)"
  , "\tcp settings lib/settings"
  , "\tcp -R bin $(prefix)/"
  , "\tcp -R lib $(prefix)/"
  ]
