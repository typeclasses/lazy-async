{ mkDerivation, base, distributive, lib }:
mkDerivation {
  pname = "data-functor-logistic";
  version = "0.0";
  sha256 = "15f794f13d4b0c7e7f89aa7c42fa7ff3cbb77a7a1a8168da8343e953af24594c";
  revision = "1";
  editedCabalFile = "135950pg0gxqhzmvzq9lmvvy5qnsd3q8963fslasgj01bixb3k0f";
  libraryHaskellDepends = [ base distributive ];
  description = "Updatable analogue of Distributive functors";
  license = lib.licenses.bsd3;
}
