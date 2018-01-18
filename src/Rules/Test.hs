module Rules.Test (testRules) where

import Base
import Expression
import Types.Flavour
import Oracles.Flag
import Oracles.Setting
import Settings
import Target
import Utilities
import GHC.Packages

import Data.Char (isSpace)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv)

lookupOnlyTests :: Action [String]
lookupOnlyTests = liftIO $ (++)
  <$> fmap (maybe [] words) (lookupEnv "TESTS")
  <*> fmap (maybe [] words) (lookupEnv "TEST")

-- FIXME: add ".exe" extension on windows
timeoutProgPath :: FilePath
timeoutProgPath = "test" -/- "bin" -/- "timeout"

timeoutPyPath :: FilePath
timeoutPyPath = "test" -/- "bin" -/- "timeout.py"

testsuiteConfigPath :: FilePath
testsuiteConfigPath = "test" -/- "config" -/- "ghc"

generateTestConfigFrom :: FilePath -> FilePath -> IO ()
generateTestConfigFrom src dest = do
  cfglines <- lines <$> readFile src
  let newcfg = unlines $ filter (not . blacklistedLine) cfglines
  createDirectoryIfMissing True (takeDirectory dest)
  writeFile dest newcfg

  where blacklistedLine = isConfigFor ["compiler", "haddock", "hp2ps", "hpc"]
        isConfigFor xs s
          | "config." `isPrefixOf` s = takeWhile (/='=') (drop 7 $ filter (not . isSpace) s) `elem` xs
          | otherwise = False

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    root <- buildRootRules

    root -/- timeoutPyPath ~> do -- FIXME: run only when not on Windows
      copyFile "testsuite/timeout/timeout.py" (root -/- timeoutPyPath)

    root -/- timeoutProgPath ~> do
      need [ root -/- timeoutPyPath ]
      let script = unlines
            [ "#!/usr/bin/env sh"
            , "exec python3 $0.py \"$@\""
            ]
      liftIO $ do
        writeFile (root -/- timeoutProgPath) script
        cmd "chmod" [ "+x", root -/- timeoutProgPath ]

    root -/- testsuiteConfigPath ~> do
      putBuild "Generating testsuite config..."
      liftIO $ generateTestConfigFrom "testsuite/config/ghc"
                                      (root -/- testsuiteConfigPath)

    "validate" ~> do
        needBuilder $ Ghc CompileHs Stage2
        needBuilder $ GhcPkg Update Stage1
        needBuilder Hpc
        need [ root -/- timeoutProgPath ]
        need [ root -/- testsuiteConfigPath ]
        -- TODO: Figure out why @needBuilder Hsc2Hs@ doesn't work.
        -- TODO: Eliminate explicit filepaths.
        -- See https://github.com/snowleopard/hadrian/issues/376.
        need ["inplace/bin/hp2ps", "inplace/bin/hsc2hs"]
        build $ target (vanillaContext Stage2 compiler) (Make "testsuite/tests") [] []

    "test" ~> do
        pkgs     <- stagePackages Stage1
        tests    <- filterM doesDirectoryExist $ concat
                    [ [ pkgPath pkg -/- "tests", pkgPath pkg -/- "tests-ghc" ]
                    | pkg <- pkgs, isLibrary pkg, pkg /= rts, pkg /= libffi ]
        windows  <- windowsHost
        let darwin = False -- FIXME
        top      <- topDirectory
        compiler <- builderPath $ Ghc CompileHs Stage2
        ghcPkg   <- builderPath $ GhcPkg Update Stage1
        haddock  <- builderPath (Haddock BuildPackage)
        threads  <- shakeThreads <$> getShakeOptions
        debugged <- ghcDebugged <$> flavour
        ghcWithNativeCodeGenInt <- fromEnum <$> ghcWithNativeCodeGen
        ghcWithInterpreterInt   <- fromEnum <$> ghcWithInterpreter
        ghcUnregisterisedInt    <- fromEnum <$> flag GhcUnregisterised
        onlyTests <- lookupOnlyTests
        need [ root -/- timeoutProgPath ]
        need [ root -/- testsuiteConfigPath ]

        let ifMinGhcVer v opt = do ver <- ghcCanonVersion
                                   if v <= v then pure opt
                                             else pure ""

        -- Prepare extra flags to send to the Haskell compiler.
        -- TODO: read extra argument for test from command line, like `-fvectorize`.
        let ghcExtraFlags = if ghcUnregisterisedInt /= 0            -- Value EXTRA_HC_OPTS should be handled.
                               then "-optc-fno-builtin"
                               else ""

        -- Take flags to send to the Haskell compiler from test.mk.
        -- See: https://github.com/ghc/ghc/blob/cf2c029ccdb967441c85ffb66073974fbdb20c20/testsuite/mk/test.mk#L37-L55
        ghcFlags <- sequence
            [ pure "-dcore-lint -dcmm-lint -no-user-package-db -rtsopts"
            , pure ghcExtraFlags
            , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
            , ifMinGhcVer "711" "-fshow-warning-groups"
            , ifMinGhcVer "801" "-fdiagnostics-color=never"
            , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
            , pure "-dno-debug-output"
            ]

        quietly . cmd "python3" $
            [ "testsuite/driver/runtests.py" ]
            ++ map ("--rootdir="++) tests ++
            [ "-e", "windows=" ++ show windows
            , "-e", "darwin=" ++ show darwin
            , "-e", "config.local=True" -- FIXME? do we ever not want to put test artefacts in tmp? see testsuite/driver/runtests.py, line 237 onwards
            , "-e", "config.cleanup=False" -- FIXME?
            , "-e", "config.speed=2"
            ] ++ concat [ ["--only", t] | t <- onlyTests ] ++
            [ "-e", "ghc_compiler_always_flags=" ++ quote (unwords ghcFlags)
            , "-e", "ghc_with_native_codegen=" ++ show ghcWithNativeCodeGenInt
            , "-e", "ghc_debugged=" ++ show (yesNo debugged)
            , "-e", "ghc_with_vanilla=1" -- TODO: do we always build vanilla?
            , "-e", "ghc_with_dynamic=0" -- TODO: support dynamic
            , "-e", "ghc_with_profiling=0" -- TODO: support profiling
            , "-e", "ghc_with_interpreter=" ++ show ghcWithInterpreterInt
            , "-e", "ghc_unregisterised=" ++ show ghcUnregisterisedInt
            , "-e", "ghc_with_threaded_rts=0" -- TODO: support threaded
            , "-e", "ghc_with_dynamic_rts=0" -- TODO: support dynamic
            , "-e", "config.ghc_dynamic_by_default=False" -- TODO: support dynamic
            , "-e", "config.ghc_dynamic=0" -- TODO: support dynamic
            , "-e", "ghc_with_llvm=0" -- TODO: support LLVM
            , "-e", "config.in_tree_compiler=True" -- TODO: when is it equal to False?
            , "-e", "clean_only=False" -- TODO: do we need to support True?
            , "--config-file=" ++ (root -/- testsuiteConfigPath)
            , "--config", "compiler="     ++ show (top -/- compiler)
            , "--config", "ghc_pkg="      ++ show (top -/- ghcPkg)
            , "--config", "haddock="      ++ show (top -/- haddock)
            , "--config", "timeout_prog=" ++ show (top -/- timeoutProgPath)

            , "--config", "a=b" -- FIXME
            , "--summary-file", "testsuite_summary.txt"
            , "--threads=" ++ show threads
            ]

            -- , "--config", "hp2ps="    ++ quote ("hp2ps")
            -- , "--config", "hpc="      ++ quote ("hpc")
            -- , "--config", "gs=$(call quote_path,$(GS))"
            -- , "--config", "timeout_prog=$(call quote_path,$(TIMEOUT_PROGRAM))"
