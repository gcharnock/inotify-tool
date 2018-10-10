# { hspkgs ? (import <nixpkgs> { }).haskellPackages }:
{ hspkgs ? (import <nixpkgs> { }).haskell.packages.ghc822 }:
hspkgs.callPackage ./inotify-tool.nix { }
