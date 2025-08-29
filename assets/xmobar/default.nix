{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, shell ? false }:

let
  inherit (pkgs) stdenv lib;
  inherit (haskellPackages) ghcWithPackages;
in haskellPackages.mkDerivation {
  pname = "custom-xmobar";
  version = "0.1.0";
  src = ./.;
  isExecutable = !shell;
  buildDepends = with haskellPackages; [
    (pkgs.pango)
    (pkgs.pkg-config)
    xmobar
    base
    alsa-core
    alsa-mixer
    xmonad
  ];
  license = [./LICENSE];
  mainProgram = "xmobar";
}
