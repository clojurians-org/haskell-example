{ pkgs ? import <nixpkgs> {}} : 

let 
  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;

    modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [pkgs.haskellPackages.cabal-install] ;
    }) ;
  } ;
in
 pkg.overrideAttrs(attr: {
   buildInputs = [ pkgs.oracle-instantclient ];
 })


