{ pkgs ? import <nixos> {} }:
pkgs.stdenv.mkDerivation rec {
  name = "lgj20";
  src = ./.;
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    pkgs.ncurses
    pkgs.sbcl
  ];
  # So sbcl can link ncurses
  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath buildInputs}";
}
