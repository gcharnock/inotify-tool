{ mkDerivation, base, criterion, deepseq, doctest, hspec
, MonadRandom, QuickCheck, random, stdenv
}:
mkDerivation {
  pname = "fused-effects";
  version = "0.2.0.0";
  sha256 = "aea82c6469d1af04a370b5545a6d2875bac2a8d07f5d68cb61a7fce823a35929";
  libraryHaskellDepends = [ base deepseq MonadRandom random ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/robrix/fused-effects";
  description = "A fast, flexible, fused effect system";
  license = stdenv.lib.licenses.bsd3;
}
