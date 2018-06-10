{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, array, base, base16-bytestring, binary
      , bytestring, containers, data-binary-ieee754, directory, entropy
      , extra, fail, hashable, hspec, lens, lens-aeson, mtl, pure-zlib
      , QuickCheck, scientific, semigroups, stdenv, tagged
      , template-haskell, text, transformers, unordered-containers
      , vector
      }:
      mkDerivation {
        pname = "avro";
        version = "0.3.0.4";
        src = ./.;
        libraryHaskellDepends = [
          aeson array base base16-bytestring binary bytestring containers
          data-binary-ieee754 entropy fail hashable mtl pure-zlib scientific
          semigroups tagged template-haskell text unordered-containers vector
        ];
        testHaskellDepends = [
          aeson array base base16-bytestring binary bytestring containers
          directory entropy extra fail hashable hspec lens lens-aeson mtl
          pure-zlib QuickCheck scientific semigroups tagged template-haskell
          text transformers unordered-containers vector
        ];
        homepage = "https://github.com/GaloisInc/avro.git#readme";
        description = "Avro serialization support for Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
