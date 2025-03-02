{ pkgs }:
pkgs.stdenv.mkDerivation {
  name = "sddm-theme";
  src = pkgs.fetchFromGitHub {
    owner = "stepanzubkov";
    repo = "where-is-my-sddm-theme";
    rev = "f9a49dd80c8a30967b77ed784eb2312a02fabcf1";
    sha256 = "1dmcgrpy8h4zg08207969pxd55yc62mbvvbi5gsc24qjwf0kdi1w";
  };
  installPhase = ''
    mkdir -p $out
    cp -R ./where_is_my_sddm_theme_qt5/* $out/
  '';
}
