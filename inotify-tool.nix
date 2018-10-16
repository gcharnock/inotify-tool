{ mkDerivation, base, bytestring, cryptonite, directory, filepath
, hashable, hashtables, hinotify, interpolate, memory, mtl, network
, pipes-network, rawfilepath, stdenv, streamly, text, transformers
, unliftio, unliftio-core, utf8-string
}:
mkDerivation {
  pname = "inotify-tool";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cryptonite directory filepath hashable hashtables
    hinotify interpolate memory mtl network pipes-network rawfilepath
    streamly text transformers unliftio unliftio-core utf8-string
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
