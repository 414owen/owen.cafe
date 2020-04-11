# <unstable> used because clay is currently broken in <nixpkgs>
{ nixpkgs ? import <unstable>, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs {} ) pkgs;

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
      blaze-html = hsSelf.callCabal2nix "blaze-html" (pkgs.fetchgit {
        url = "https://github.com/jaspervdj/blaze-html.git";
        rev = "0fad644e86943ec309f0b3a0a5a01c59a64243d4";
        sha256 = "1axz4r336vywyfla2drz5pbasn1vqzaqz0w8r2i8r4gxx9psy68s";
      }) {};
      clay = hsSelf.callCabal2nix "clay" (pkgs.fetchgit {
        url = "https://github.com/sebastiaanvisser/clay.git";
        rev = "3808e460c044809f088ac8952ed3720cf801daf3";
        sha256 = "3c6e14676659fd8b1dacdb7cc8544b18200afc2bd37c4d597fc15c2e0421b4c9";
      }) {};
    };
  };

  # h = haskellPackages.extend (self: super: {
  #   clay = self.callHackage "clay" "0.13.2" {};
  # });

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (h.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
