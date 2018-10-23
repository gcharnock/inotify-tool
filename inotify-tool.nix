{ mkDerivation, aeson, base, bytestring, cryptonite, directory
, filepath, hashable, hashtables, hinotify, interpolate, memory
, mtl, network, optparse-applicative, rawfilepath, stdenv, text
, transformers, unliftio, unliftio-core, utf8-string
}:
mkDerivation {
  pname = "inotify-tool";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base bytestring text ];
  executableHaskellDepends = [
    aeson base bytestring cryptonite directory filepath hashable
    hashtables hinotify interpolate memory mtl network
    optparse-applicative rawfilepath text transformers unliftio
    unliftio-core utf8-string
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
