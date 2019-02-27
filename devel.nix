# { hspkgs ? (import <nixpkgs> { }).haskellPackages }:
let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  dontCheck = pkgs.haskell.lib.dontCheck;
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc844 = pkgs.haskell.packages.ghc844.override {
            overrides = self: super: {
              contextual-logger = self.callPackage ../contextual-logger/haskell {};
              fused-effects = dontCheck (self.callPackage ./fused-effects.nix {});
              inotify-tool = dontCheck (self.callPackage ./. {});
            };
          };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
  hsPkgs = pkgs.haskell.packages.ghc844;
in
{ 
  inherit pkgs;
  build = hsPkgs.inotify-tool;
  env = hsPkgs.inotify-tool.env;
}
