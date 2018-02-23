module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import GHC.Packages
import Settings.Builders.Common

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage   <- getStage
    root    <- getBuildRoot
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    mconcat [ getSettingList ConfHsCppArgs
            , arg "-P"
            , arg "-Iincludes"
            , arg $ "-I" ++ root -/- generatedDir
            , arg $ "-I" ++ ghcPath
            , arg "-x", arg "c"
            , arg =<< getInput ]
