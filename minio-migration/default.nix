{ pkgs ? import (builtins.fetchTarball {url="https://github.com/NixOS/nixpkgs/archive/2c07921cff84dfb0b9e0f6c2d10ee2bfee6a85ac.tar.gz";}) {}} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # clntsh = oracle-instantclient ;
      minio-hs = dontCheck super.minio-hs ;
      # conduit = doJailbreak super.conduit ;
    } ;
  } ;
  pkg = haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  } ;
  buildInputs = [ ] ;
in pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs ;
})
