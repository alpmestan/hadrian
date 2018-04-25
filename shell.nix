# Invoking nix-shell sets up an environment where we can build ghc
# by only invoking hadrian.


{ _nixpkgs ? import <nixpkgs> {}
, bootghc ? "ghc822"
}:

let

  nixpkgs = import (_nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "e7a327da5cffdf5e77e1924906a4f0983591bd3e";
    sha256 = "1xzil4mayhggg2miwspbk12nihlszg0y4n6i4qacrxql5n75f0hr";
  }){ overlays = [cabalHashes]; };



  cabalHashes = sel: super: {
    all-cabal-hashes = super.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b2b93ae610f5f1b51d22b191f972dc3dec8f94c6.tar.gz";
      sha256 = "0bffclpqbw62xff36qlzxghr042mhv0m06k5ml4298w6fv7ly1xw";
    };
  };

  haskellPackages = nixpkgs.haskell.packages.${bootghc};

  removeBuild = path: type:
    let baseName = baseNameOf (toString path);
    in
        ! (baseName == "_build"
           || baseName == "dist"
           || baseName == "dist-newstyle"
           || baseName == ".stack-work"
           || baseName == "config.log"
           || baseName == "config.status"
           || baseName == "shell.nix"
           || nixpkgs.lib.hasSuffix ".sh" baseName
           || !(nixpkgs.lib.cleanSourceFilter path type)) ;

  filterSrc = path: builtins.filterSource removeBuild path;
  dontTest = nixpkgs.haskell.lib.dontCheck;

  hadrianPackages = nixpkgs.haskell.packages.${bootghc}.override {
    overrides = self: super: let
        localPackage = name: path: self.callCabal2nix name (filterSrc path) {};
      in {
        hadrian = localPackage "hadrian" ./. ;
        happy = dontTest (super.happy);
        shake = dontTest (self.callHackage "shake" "0.16.2" {});
        extra = dontTest (self.callHackage "extra" "1.6.4" {});
        QuickCheck = dontTest (self.callHackage "QuickCheck" "2.10" {});
        Cabal = dontTest (localPackage "Cabal" ./../libraries/Cabal/Cabal) ;
        filepath = dontTest (localPackage "filepath" ./../libraries/filepath) ;
        text = dontTest (localPackage "text" ./../libraries/text)  ;
        hpc = dontTest (localPackage"hpc" ./../libraries/hpc) ;
        parsec = dontTest (localPackage "parsec" ./../libraries/parsec) ;
        HUnit = dontTest (self.callHackage "HUnit" "1.3.1.2" {});
        process = dontTest (localPackage "process" ./../libraries/process) ;
        directory = dontTest (localPackage "directory" ./../libraries/directory) ;
      }; };

  cabalPackages = nixpkgs.haskell.packages.${bootghc}.override {
    overrides = self: super: let
        localPackage = name: path: self.callCabal2nix name (filterSrc path) {};
      in {
        Cabal = localPackage "Cabal" ./../../cabal/Cabal ;
        cabal-install = self.callPackage ./../../cabal/cabal-install.nix {};
      }; };


in
  nixpkgs.lib.overrideDerivation nixpkgs.haskell.packages.ghcHEAD.ghc
    (drv: {
      name = "ghc-dev";
      nativeBuildInputs = drv.nativeBuildInputs ++
        [ hadrianPackages.hadrian
          nixpkgs.arcanist
          nixpkgs.git
          nixpkgs.python3Packages.sphinx
          nixpkgs.texlive.combined.scheme-basic
          (haskellPackages.ghcWithPackages
            (ps: [ps.html ps.regex-compat ps.dump-core]))

          #cabalPackages.cabal-install
        ];
    })
