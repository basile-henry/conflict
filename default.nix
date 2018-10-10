{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:

with nixpkgs.pkgs.haskell;

lib.failOnAllWarnings (packages.${compiler}.callCabal2nix "conflict" ./.  {
  megaparsec = packages.${compiler}.megaparsec_7_0_1;
})
