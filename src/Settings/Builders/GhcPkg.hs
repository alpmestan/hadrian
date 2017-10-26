module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = mconcat
    [ builder (GhcPkg Init) ? mconcat [ arg "init", arg =<< getOutput ]

    , builder (GhcPkg Update) ? do
        verbosity <- expr getVerbosity
        context   <- getContext
        config    <- expr $ pkgInplaceConfig context
        stage     <- getStage
        pkgDb     <- expr $ packageDbPath stage
        mconcat [ notStage0 ? arg "--global-package-db"
                , notStage0 ? arg pkgDb
                , arg "update"
                , arg "--force"
                , verbosity < Chatty ? arg "-v0"
                , bootPackageDatabaseArgs
                , arg config ] ]
