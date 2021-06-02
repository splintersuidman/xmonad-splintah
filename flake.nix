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
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in {

        devShell = self.packages.${system}.xmonad-splintah.env;
        packages.xmonad-splintah =
          haskellPackages.callPackage ./xmonad-splintah/xmonad-splintah.nix { };

        defaultPackage = self.packages.${system}.xmonad-splintah;

        # Build script for XMonad.
        apps.build = {
          type = "app";
          program = let
            build = pkgs.writeScriptBin "build" ''
              #!${pkgs.stdenv.shell}
              dist=$1
              cp ${self.defaultPackage.${system}}/bin/xmonad-splintah "$dist"
              chmod a+w "$dist"
            '';
          in "${build}/bin/build";
        };

        defaultApp = self.apps.${system}.build;
      });
}
