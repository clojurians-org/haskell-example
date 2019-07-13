{ pkgs ? import <nixpkgs> {}} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # tasty = dontCheck (doJailbreak super.tasty) ;
      # minio-hs = dontCheck (doJailbreak (self.callCabal2nix "minio-hs"
      #   (pkgs.fetchFromGitHub {
      #     owner = "minio" ;
      #     repo = "minio-hs" ;
      #     rev = "v1.3.1" ;
      #     sha256 = "0714dg4fqdqcf0w2a4wcp45xz1jbv04qmpgcgdr498wpks7kkjpm" ;
      #   }) {})) ;
      minio-hs = dontCheck (doJailbreak (self.callCabal2nix "minio-hs" ./minio-hs {})) ;
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

