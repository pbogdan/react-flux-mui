{ mkDerivation, aeson, aeson-casing, base, protolude, react-flux
, stdenv, unordered-containers
}:
mkDerivation {
  pname = "react-flux-mui";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-casing base protolude react-flux unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
