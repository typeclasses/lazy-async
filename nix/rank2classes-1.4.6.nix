{ mkDerivation, base, Cabal, cabal-doctest, data-functor-logistic
, distributive, doctest, lib, markdown-unlit, tasty, tasty-hunit
, template-haskell, transformers
}:
mkDerivation {
  pname = "rank2classes";
  version = "1.4.6";
  sha256 = "9125c012242c947b058f27d42abe8cc65f350bb22d907d7866d15420ac929727";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base data-functor-logistic distributive template-haskell
    transformers
  ];
  testHaskellDepends = [
    base data-functor-logistic distributive doctest tasty tasty-hunit
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://github.com/blamario/grampa/tree/master/rank2classes";
  description = "standard type constructor class hierarchy, only with methods of rank 2 types";
  license = lib.licenses.bsd3;
}
