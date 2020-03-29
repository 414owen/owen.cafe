{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  inputsFrom = with pkgs; [ (import ./shell.nix {}) ];
  buildInputs = with pkgs; [ inotify-tools bc stdenv psmisc ];
}
