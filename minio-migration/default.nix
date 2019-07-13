# nix-channel --add https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz nixpkgs-static
# nix-channel --update nixpkgs-static
# export NIX_PATH=nixpkgs-static=/home/op/.nix-defexpr/channels/nixpkgs-static

{ nixpkgs ? (import <nixpkgs-static> {
              config.packageOverrides = pkgs: rec {
                nix = pkgs.nix.overrideDerivation (old: {
                  doInstallCheck = false ;
                }) ;
              } ;
            }).pkgsMusl} :

with nixpkgs;
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
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;

      isLibrary = false;
      isExecutable = true;
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--disable-executable-stripping"
      ];

    }) ;
  }
