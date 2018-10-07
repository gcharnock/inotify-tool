{ hspkgs ? (import <nixpkgs> { }).haskellPackages }:
hspkgs.callPackage ./inotify-tool.nix { }
