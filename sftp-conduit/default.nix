{ nixpkgs ? import <nixpkgs> {}} :

with nixpkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hlibssh2 = dontCheck (doJailbreak (
        addBuildDepends 
          (addPkgconfigDepends (self.callCabal2nix "hlibssh2" ./dep/libssh2-hs/hlibssh2 {}) [libssh2] )
          [libssh2]
      )) ;
    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  }

