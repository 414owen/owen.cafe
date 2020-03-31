# <unstable> used because clay is currently broken in <nixpkgs>
{ nixpkgs ? import <unstable> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-html, blaze-svg, bytestring, clay, directory
      , filepath, mime-types, optparse-applicative, scotty, stdenv, text
      }:
      mkDerivation {
        pname = "buss";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base blaze-html blaze-svg bytestring clay directory filepath mime-types
          optparse-applicative scotty text
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  h = haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      clay = pkgs.haskell.lib.overrideCabal hsSuper.clay (oa: {
        version = "0.13.3";
        sha256 = "192lsbyj6azjs2ygpx4i47fyr8zfmvwcas8mia07ndqglk2c9csx";
      });
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (h.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
