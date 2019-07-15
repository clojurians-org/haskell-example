{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    labels-json = hackGet ./dep/labels-json ;
  } ;
  overrides = self: super: {
    servant = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant"
      (pkgs.fetchFromGitHub {
        owner = "haskell-servant" ;
        repo = "servant" ;
        rev = "d4289931ad69f1233c9f75f230f7bb29650df433" ;
        sha256 = "019w6h695vi83hsncrqvkf9ykg8zidwn4z1aaf2wz48n39hcizwc" ;} + /servant)
      {})) ;

    servant-server = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant-server"
      (pkgs.fetchFromGitHub {
        owner = "haskell-servant" ;
        repo = "servant" ;
        rev = "d4289931ad69f1233c9f75f230f7bb29650df433" ;
        sha256 = "019w6h695vi83hsncrqvkf9ykg8zidwn4z1aaf2wz48n39hcizwc" ;} + /servant-server)
      {})) ;

    servant-snap = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant-snap"
      (pkgs.fetchFromGitHub {
        owner = "haskell-servant" ;
        repo = "servant-snap" ;
        rev = "5ed901641c99519fa3e0c701cb430dbb8f6f1b5c" ;
        sha256 = "01w5b4syq775d5pq04cbclvq58wgpimqc22pwkq54yhynmvh7rpq" ;}) 
      {})) ;

    semantic-reflex = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "semantic-reflex"
      (pkgs.fetchFromGitHub {
        owner = "tomsmalley" ;
        repo = "semantic-reflex" ;
        rev = "a354fda1f34d06b72fd99dea1206606b5210ecdd" ;
        sha256 = "1li8w95ibq4xm717clz5wz23kdp15j9vrqb1kq64d5ld0fjx7ln0" ;} + /semantic-reflex)
      {}))) ;

    # reflex-dom-contrib = pkgs.haskell.lib.doJailbreak super.reflex-dom-contrib;
  } ;
})
