{ mkDerivation, aeson, base, binary, bytestring, cryptonite
, directory, filepath, hashable, hashtables, hinotify, interpolate
, memory, mtl, network, optparse-applicative, pipes, pipes-binary
, pipes-bytestring, pipes-parse, rawfilepath, stdenv, text
, transformers, unix, unliftio, unliftio-core, utf8-string
}:
mkDerivation {
  pname = "inotify-tool";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base bytestring text ];
  executableHaskellDepends = [
    aeson base binary bytestring cryptonite directory filepath hashable
    hashtables hinotify interpolate memory mtl network
    optparse-applicative pipes pipes-binary pipes-bytestring
    pipes-parse rawfilepath text transformers unix unliftio
    unliftio-core utf8-string
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
