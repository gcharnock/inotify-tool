# { hspkgs ? (import <nixpkgs> { }).haskellPackages }:
{ hspkgs ? (import <nixpkgs> { }).haskell.packages.ghc844 }:
let dontCheck = (import <nixpkgs> {}).haskell.lib.dontCheck; in
dontCheck (hspkgs.callPackage ./inotify-tool.nix { })
