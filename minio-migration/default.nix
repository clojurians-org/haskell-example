{ pkgs ? (import (builtins.fetchTarball {url="https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz";}) {}).pkgsMusl} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # conduit = doJailbreak super.conduit ;
      # nix-fetch-url https://github.com/bos/criterion/releases/tag/criterion-1.5.5.0

      # minio-hs = dontCheck (doJailbreak (self.callCabal2nix "minio-hs"
      #   (pkgs.fetchFromGitHub {
      #     owner = "minio" ;
      #     repo = "minio-hs" ;
      #     rev = "v1.2.0" ;
      #     sha256 = "1ybihlmbcsl4biak4b2wbl7dz2n03mar4hvhz148mkfal9dr9agw" ;
      #   }) {})) ;
    } ;
  } ;
  pkg = haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;

      isLibrary = false;
      isExecutable = true;
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--disable-executable-stripping"
      ];

    }) ;
  } ;
  buildInputs = [ ] ;
in pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs ;
})
