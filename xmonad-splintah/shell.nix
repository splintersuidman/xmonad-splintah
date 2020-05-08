{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "ghc865" }:
(nixpkgs.callPackage ./default.nix { inherit compiler; }).env
