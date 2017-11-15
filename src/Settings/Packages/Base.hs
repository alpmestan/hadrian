module Settings.Packages.Base (basePackageArgs) where

import Expression
import Settings
import GHC.Packages

basePackageArgs :: Args
basePackageArgs = package base ? do
    integerLibraryName <- pkgName <$> getIntegerPackage
    mconcat [ builder CabalFlags ? arg ('+':integerLibraryName)
            -- This fixes the 'unknown symbol stat' issue.
            -- See: https://github.com/snowleopard/hadrian/issues/259.
            , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]
