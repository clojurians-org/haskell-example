{ pkgs ? import <nixpkgs> {}} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      minio-hs = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "minio-hs"
        (pkgs.fetchFromGitHub {
          owner = "minio" ;
          repo = "minio-hs" ;
          rev = "1e6579b02b2992ed74f4df5fa5d24f90ec3a63d5" ;
          sha256 = "1fs055hj46mjvmq1jfs48skclxfv431mnihjaqnmd2qvja23yvmk" ;
      }) {})) ;

      labels-json = dontCheck (doJailbreak (self.callCabal2nix "labels-json"
        (pkgs.fetchFromGitHub {
          owner = "chrisdone" ;
          repo = "labels" ;
          rev = "v0.3.3" ;
          sha256 = "1hjvj75axc8ph49gwh58gck6vr9h57g04pgx3x8i9b90m0ig1iri" ;
        } + /labels-json ) {})) ;
    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  }

