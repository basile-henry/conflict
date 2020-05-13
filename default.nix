{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "ghc865" }:

with nixpkgs.pkgs.haskell;

lib.failOnAllWarnings (packages.${compiler}.callCabal2nix "conflict" ./.  {})
