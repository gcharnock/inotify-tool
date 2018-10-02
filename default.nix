let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./inotify-tool.nix { }
