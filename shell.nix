# <unstable> used because clay is currently broken in <nixpkgs>
{ nixpkgs ? import <unstable> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-html, bytestring, clay, directory
      , filepath, mime-types, optparse-applicative, scotty, stdenv, text
      }:
      mkDerivation {
        pname = "buss";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base blaze-html bytestring clay directory filepath mime-types
          optparse-applicative scotty text
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  # h = haskellPackages.extend (self: super: {
  #   clay = self.callHackage "clay" "0.13.2" {};
  # });
  h = pkgs.haskellPackages;

  drv = variant (h.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
