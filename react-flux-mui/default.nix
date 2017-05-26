{ compiler ? "ghc802"
}:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${compiler}.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          react-flux =
            haskellPackagesNew.callPackage ./deps/react-flux.nix {};
          react-flux-mui =
            haskellPackagesNew.callPackage ./react-flux-mui.nix {
              react-flux = react-flux;
          };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  pkgs
  .haskellPackages
  .react-flux-mui
