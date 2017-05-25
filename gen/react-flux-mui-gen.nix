{ mkDerivation, aeson, base, bytestring, directory, ede, lens, mtl
, protolude, react-docgen-types, reflection, stdenv, text
, typelits-witnesses, unordered-containers
}:
mkDerivation {
  pname = "react-flux-mui-gen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory ede lens mtl protolude
    react-docgen-types reflection text typelits-witnesses
    unordered-containers
  ];
  executableHaskellDepends = [ base protolude ];
  license = stdenv.lib.licenses.bsd3;
}
