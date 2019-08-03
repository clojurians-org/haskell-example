{ pkgs ? import <nixpkgs> {}} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      # clntsh = oracle-instantclient ;
    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install haskellPackages.c2hs] ;
      librarySystemDepends = [ xgboost ] ;
    }) ;
  }
