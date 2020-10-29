{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "ghc884" }:
(nixpkgs.callPackage ./default.nix { inherit compiler; }).env
