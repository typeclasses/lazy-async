{ mkDerivation, base, distributive, fetchgit, lib }:
mkDerivation {
  pname = "data-functor-logistic";
  version = "0.0";
  src = fetchgit {
    url = "https://github.com/chris-martin/data-functor-logistic.git";
    sha256 = "052yb1b182x86ri0vv8g7v3q8c959rmlqv8x8yiif2hp5ckn49ff";
    rev = "82b89caa5f71235ac7911ca306114f33e23308f9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base distributive ];
  description = "Updatable analogue of Distributive functors";
  license = lib.licenses.bsd3;
}
