{
  description = ''
    My xmonad build for NixOS. It is built using Nix and Cabal using Nix flakes
    (see <https://nixos.wiki/wiki/Flakes>).

    Copyright (C) 2021 Splinter Suidman

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU Affero General Public License as published by the
    Free Software Foundation, either version 3 of the License, or (at your
    option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
    for more details.
  '';

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc8102";
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.xmonad-splintah =
          pkgs.pkgs.haskell.packages.${compiler}.callPackage
          ./xmonad-splintah/xmonad-splintah.nix { };

        devShell = self.packages.${system}.xmonad-splintah.env;

        defaultPackage = self.packages.${system}.xmonad-splintah;
      });
}
