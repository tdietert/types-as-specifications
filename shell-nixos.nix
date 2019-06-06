{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, ghcid, hpack, indexed
      , indexed-extras, row-types, cabal-install, stdenv
      }:
      mkDerivation {
        pname = "types-as-specifications";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring ghcid indexed cabal-install (pkgs.haskell.lib.doJailbreak indexed-extras) row-types
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base bytestring ghcid indexed cabal-install (pkgs.haskell.lib.doJailbreak indexed-extras) row-types
        ];
        testHaskellDepends = [
          base bytestring ghcid indexed cabal-install (pkgs.haskell.lib.doJailbreak indexed-extras) row-types
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/githubuser/types-as-specifications#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
