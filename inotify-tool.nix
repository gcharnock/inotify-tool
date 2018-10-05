{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cryptonite, directory
      , filepath, hashtables, hinotify, interpolate, memory, mtl
      , rawfilepath, stdenv, text, transformers, unliftio, unliftio-core
      , utf8-string
      }:
      mkDerivation {
        pname = "inotify-tool";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring cryptonite directory filepath hashtables hinotify
          interpolate memory mtl rawfilepath text transformers unliftio
          unliftio-core utf8-string
        ];
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
