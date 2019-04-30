{ pkgs ? import <nixpkgs> {}} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      # clntsh = oracle-instantclient ;
    } ;
  } ;
  pkg = haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install haskellPackages.c2hs] ;
      librarySystemDepends = [ odpic ] ;
    }) ;

  } ;
  # buildInputs = [ oracle-instantclient ] ;
  buildInputs = [ odpic ] ;
in pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs ;
})
