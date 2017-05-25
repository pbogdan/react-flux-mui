{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
nixpkgs
  .pkgs
  .haskell
  .packages
  .${compiler}
  .callPackage ./react-flux-mui-gen.nix {
    react-docgen-types = nixpkgs
      .pkgs
      .haskell
      .packages
      .${compiler}
      .callPackage ../../react-docgen-types {};
  }
