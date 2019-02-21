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
  hoogle = hsPkgs.ghcWithHoogle (hspkgs: [
    hspkgs.aeson
    hspkgs.base
    hspkgs.binary
    hspkgs.bytestring
    hspkgs.cryptonite
    hspkgs.directory
    hspkgs.filepath
    hspkgs.hashable
    hspkgs.hashtables
    hspkgs.hinotify
    hspkgs.interpolate
    hspkgs.memory
    hspkgs.mtl
    hspkgs.network
    hspkgs.pipes
    hspkgs.pipes-binary
    hspkgs.pipes-bytestring
    hspkgs.pipes-parse
    hspkgs.rainbow
    hspkgs.rawfilepath
    hspkgs.stm
    hspkgs.text
    hspkgs.transformers
    hspkgs.unix
    hspkgs.unliftio
    hspkgs.unliftio-core
    hspkgs.utf8-string
    hspkgs.contextual-logger
  ]);
}
