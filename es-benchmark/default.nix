{ pkgs ? import <nixpkgs> {}} : pkgs.haskellPackages.developPackage {
  root = ./.;

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [pkgs.haskellPackages.cabal-install] ;
  }) ;
}
