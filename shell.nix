{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "types-as-specifications";
  buildInputs = [ glpk pcre ];
}
